--------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.AST.Executor where

import           Control.Monad.State        hiding (mapM, mapM_)
import           Control.Monad.Trans.Either
import qualified Data.Char                  as Char
import qualified Data.Maybe                 as Maybe
import qualified Text.Read                  as Read

import           Flowbox.Control.Error                      (catchEither, hoistEitherWith)
import qualified Flowbox.Data.List                          as List
import           Flowbox.Data.MapForest                     (MapForest)
import           Flowbox.Prelude                            as Prelude hiding (children, inside)
import           Flowbox.Source.Location                    (loc)
import           Flowbox.System.Log.Logger
import qualified Luna.Graph.Node                            as Node
import           Luna.Graph.Node.Expr                       (NodeExpr)
import qualified Luna.Graph.Node.Expr                       as NodeExpr
import qualified Luna.Graph.Node.StringExpr                 as StringExpr
import qualified Luna.Interpreter.Session.AST.Traverse      as Traverse
import qualified Luna.Interpreter.Session.Cache.Cache       as Cache
import qualified Luna.Interpreter.Session.Cache.Free        as Free
import qualified Luna.Interpreter.Session.Cache.Invalidate  as Invalidate
import qualified Luna.Interpreter.Session.Cache.Status      as CacheStatus
import qualified Luna.Interpreter.Session.Cache.Value       as Value
import qualified Luna.Interpreter.Session.Data.CallData     as CallData
import           Luna.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath as CallDataPath
import           Luna.Interpreter.Session.Data.CallPoint    (CallPoint)
import           Luna.Interpreter.Session.Data.VarName      (VarName (VarName))
import qualified Luna.Interpreter.Session.Data.VarName      as VarName
import qualified Luna.Interpreter.Session.Debug             as Debug
import qualified Luna.Interpreter.Session.Env               as Env
import qualified Luna.Interpreter.Session.Error             as Error
import qualified Luna.Interpreter.Session.Hash              as Hash
import           Luna.Interpreter.Session.Memory.Manager    (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager    as Manager
import           Luna.Interpreter.Session.ProfileInfo       (ProfileInfo)
import           Luna.Interpreter.Session.Session           (Session)
import qualified Luna.Interpreter.Session.Session           as Session
import qualified Luna.Interpreter.Session.TargetHS.Bindings as Bindings
import qualified Luna.Interpreter.Session.TargetHS.TargetHS as TargetHS
import qualified Luna.Interpreter.Session.Var               as Var
import qualified Luna.Pass.CodeGen.HSC.HSC                  as HSC
import qualified Luna.Pass.Transform.AST.Hash.Hash          as Hash
import qualified Luna.Pass.Transform.HAST.HASTGen.HASTGen   as HASTGen



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


processMain :: MemoryManager mm => Session mm (MapForest CallPoint ProfileInfo)
processMain = processMain_ >> Env.getProfileInfos


processMain_ :: MemoryManager mm => Session mm ()
processMain_ = do
    Env.cleanProfileInfos
    --TargetHS.reload
    mainPtr  <- Env.getMainPtr
    children <- CallDataPath.addLevel [] mainPtr
    mapM_ processNodeIfNeeded children
    Env.setAllReady True
    Debug.dumpBindings


processNodeIfNeeded :: MemoryManager mm => CallDataPath -> Session mm ()
processNodeIfNeeded callDataPath =
    whenM (Cache.isDirty $ CallDataPath.toCallPointPath callDataPath)
          (processNode callDataPath)


processNode :: MemoryManager mm => CallDataPath -> Session mm ()
processNode callDataPath = Env.profile (CallDataPath.toCallPointPath callDataPath) $ do
    arguments <- Traverse.arguments callDataPath
    let callData  = last callDataPath
        node      = callData ^. CallData.node
    varNames <- mapM (Cache.recentVarName . CallDataPath.toCallPointPath) arguments

    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  {} ->
                return ()
            Node.Outputs {} ->
                executeOutputs callDataPath varNames
            Node.Expr (NodeExpr.StringExpr (StringExpr.Pattern {})) _ _ ->
                executeAssignment callDataPath varNames
            Node.Expr {}                                                ->
                executeNode       callDataPath varNames
        else mapM_ processNodeIfNeeded children


executeOutputs :: MemoryManager mm
               => CallDataPath -> [VarName] -> Session mm ()
executeOutputs callDataPath varNames = do
    let argsCount  = length $ Traverse.inDataConnections callDataPath
        nodeExpr = NodeExpr.StringExpr $ if argsCount == 1 then StringExpr.Id else StringExpr.Tuple
    when (length callDataPath > 1) $
        execute (init callDataPath) nodeExpr varNames


executeNode :: MemoryManager mm
            => CallDataPath -> [VarName] -> Session mm ()
executeNode callDataPath varNames = do
    let node       = last callDataPath ^. CallData.node
    case node of
        Node.Expr nodeExpr _ _ -> execute callDataPath nodeExpr varNames


executeAssignment :: MemoryManager mm
                  => CallDataPath -> [VarName] -> Session mm ()
executeAssignment callDataPath [varName] =
    execute callDataPath (NodeExpr.StringExpr StringExpr.Id) [varName] -- TODO [PM] : handle Luna's pattern matching


execute :: MemoryManager mm
        => CallDataPath -> NodeExpr -> [VarName] -> Session mm ()
execute callDataPath nodeExpr varNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    status       <- Cache.status        callPointPath
    prevVarName  <- Cache.recentVarName callPointPath
    boundVarName <- Cache.dependency varNames callPointPath
    let execFunction = evalFunction nodeExpr callDataPath varNames

        executeModified = do
            varName <- execFunction
            if varName /= prevVarName
                then if boundVarName /= Just varName
                    then do logger debug "processing modified node - result value differs"
                            mapM_ Free.freeVarName boundVarName
                            Invalidate.markSuccessors callDataPath CacheStatus.Modified

                    else do logger debug "processing modified node - result value differs but is cached"
                            Invalidate.markSuccessors callDataPath CacheStatus.Affected
                else if null $ varName ^. VarName.hash
                    then do logger debug "processing modified node - result value non hashable"
                            Invalidate.markSuccessors callDataPath CacheStatus.Modified
                    else logger debug "processing modified node - result value is same"

        executeAffected = case boundVarName of
            Nothing    -> do
                logger debug "processing affected node - result never bound"
                _ <- execFunction
                Invalidate.markSuccessors callDataPath CacheStatus.Modified
            Just bound -> do
                logger debug "processing affected node - result rebound"
                Cache.setRecentVarName bound callPointPath
                Value.reportIfVisible callPointPath
                Invalidate.markSuccessors callDataPath CacheStatus.Affected

    case status of
        CacheStatus.Affected     -> executeAffected
        CacheStatus.Modified     -> executeModified
        CacheStatus.NonCacheable -> executeModified
        CacheStatus.Ready        -> left $ Error.OtherError $(loc) "something went wrong : status = Ready"


data VarType = Lit      String
             | LitInt   String
             | LitFloat String
             | Con      String
             | Var      String
             | Native   String
             | Tuple
             | List
             | TimeVar
             | Id
             | Expression String
             deriving Show


varType :: NodeExpr -> Session mm VarType
varType (NodeExpr.StringExpr  StringExpr.Id                ) = return   Id
varType (NodeExpr.StringExpr  StringExpr.Tuple             ) = return   Tuple
varType (NodeExpr.StringExpr  StringExpr.List              ) = return   List
varType (NodeExpr.StringExpr (StringExpr.Native name      )) = return $ Native name
varType (NodeExpr.StringExpr (StringExpr.Expr   []        )) = return $ Prelude.error "varType : empty expression"
varType (NodeExpr.StringExpr (StringExpr.Expr   name@(h:_)))
    | Maybe.isJust (Read.readMaybe name :: Maybe Char)   = return $ Lit      name
    | Maybe.isJust (Read.readMaybe name :: Maybe Int)    = return $ LitInt   name
    | Maybe.isJust (Read.readMaybe name :: Maybe Float)  = return $ LitFloat name
    | Maybe.isJust (Read.readMaybe name :: Maybe String) = return $ Lit      name
    | name == Var.timeRef                                = return   TimeVar
    | Char.isUpper h                                     = return $ Con      name
    | otherwise                                          = return $ Var      name
varType (NodeExpr.ASTExpr expr) = do
    expr' <- Var.replaceTimeRefs expr
    -- TODO[PM] : replaceTimeVars
    hast <- hoistEitherWith (Error.OtherError $(loc)) =<< HASTGen.runExpr expr'
    return $ Expression $ HSC.genExpr hast


nameHash :: String -> String
nameHash name = if name == "Point2" then "Point2" else Hash.hashStr name


evalFunction :: MemoryManager mm
             => NodeExpr -> CallDataPath -> [VarName] -> Session mm VarName
evalFunction nodeExpr callDataPath varNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        tmpVarName    = "_tmp"
        mkArg arg = "(Value (Pure "  ++ VarName.toString arg ++ "))"
        args      = map mkArg varNames
        appArgs a = if null a then "" else " $ appNext " ++ List.intercalate " $ appNext " (reverse a)
        genNative = List.replaceByMany "#{}" args . List.stripIdx 3 3

        self      = head varNames
    vt <- varType nodeExpr
    operation <- case vt of
        List        -> return $ "toIOEnv $ fromValue $ val [" ++ List.intercalate "," args ++ "]"
        Id          -> return $ "toIOEnv $ fromValue $ " ++ mkArg self
        Native name -> return $ "toIOEnv $ fromValue $ " ++ genNative name
        Con    name -> return $ "toIOEnv $ fromValue $ call" ++ appArgs args ++ " $ cons_" ++ nameHash name
        Var    name -> return $ "toIOEnv $ fromValue $ call" ++ appArgs (tail args) ++ " $ member (Proxy::Proxy " ++ show (nameHash name) ++ ") " ++ mkArg self
        LitInt   name -> return $ "toIOEnv $ fromValue $ val (" ++ name ++ " :: Int)"
        LitFloat name -> return $ "toIOEnv $ fromValue $ val (" ++ name ++ " :: Float)"
        Lit      name -> return $ "toIOEnv $ fromValue $ val (" ++ name ++ ")"
        Tuple       -> return $ "toIOEnv $ fromValue $ val (" ++ List.intercalate "," args ++ ")"
        TimeVar     -> do time <- Env.getTimeVar
                          return $ "toIOEnv $ fromValue $ val (" ++ show time ++ " :: Float)"
        Expression  name -> return $ "toIOEnv $ fromValue $ " ++ name
    catchEither (left . Error.RunError $(loc) callPointPath) $ do
        Session.runAssignment' tmpVarName operation
        hash <- Hash.computeInherit tmpVarName varNames
        let varName = VarName callPointPath hash
        Session.runAssignment (VarName.toString varName) tmpVarName
        lift2 $ Bindings.remove tmpVarName
        Cache.put callDataPath varNames varName
        Value.reportIfVisible callPointPath
        Manager.reportUseMany varNames
        Manager.reportUse varName
        return varName
