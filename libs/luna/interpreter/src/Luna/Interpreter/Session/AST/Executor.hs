--------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Luna.Interpreter.Session.AST.Executor where

import           Control.Monad.Catch                        (SomeException)
import qualified Control.Monad.Catch                        as Catch
import           Control.Monad.State                        hiding (mapM, mapM_)
import           Control.Monad.Trans.Either
import qualified Data.Char                                  as Char
import qualified Data.List                                  as List
import qualified Data.Maybe                                 as Maybe
import qualified Data.String.Utils                          as Utils
import qualified Data.Text.Lazy                             as Text
import qualified Language.Preprocessor.Cpphs                as Cpphs
import qualified Text.Read                                  as Read

import           Flowbox.Control.Error                      (catchEither, hoistEitherWith)
import qualified Flowbox.Data.List                          as List
import           Flowbox.Data.MapForest                     (MapForest)
import           Flowbox.Prelude                            as Prelude hiding (children, inside)
import           Flowbox.Source.Location                    (loc)
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.ASTConvertion                 (convertAST)
import qualified Luna.DEP.Graph.Node                        as Node
import           Luna.DEP.Graph.Node.Expr                   (NodeExpr)
import qualified Luna.DEP.Graph.Node.Expr                   as NodeExpr
import qualified Luna.DEP.Graph.Node.StringExpr             as StringExpr
import qualified Luna.Interpreter.Session.AST.Traverse      as Traverse
import qualified Luna.Interpreter.Session.Cache.Cache       as Cache
import qualified Luna.Interpreter.Session.Cache.Invalidate  as Invalidate
import qualified Luna.Interpreter.Session.Cache.Status      as CacheStatus
import qualified Luna.Interpreter.Session.Cache.Value       as Value
import qualified Luna.Interpreter.Session.Data.CallData     as CallData
import           Luna.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath as CallDataPath
import           Luna.Interpreter.Session.Data.CallPoint    (CallPoint)
import           Luna.Interpreter.Session.Data.CompiledNode (CompiledNode (CompiledNode))
import           Luna.Interpreter.Session.Data.KeyName      (KeyName (KeyName))
import qualified Luna.Interpreter.Session.Debug             as Debug
import qualified Luna.Interpreter.Session.Env               as Env
import           Luna.Interpreter.Session.Error             (Error, mapError)
import qualified Luna.Interpreter.Session.Error             as Error
import qualified Luna.Interpreter.Session.Hint.Eval         as HEval
import           Luna.Interpreter.Session.Memory.Manager    (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager    as Manager
import           Luna.Interpreter.Session.Profile.Info      (ProfileInfo)
import qualified Luna.Interpreter.Session.Profile.Profile   as Profile
import           Luna.Interpreter.Session.Session           (Session)
import qualified Luna.Interpreter.Session.Session           as Session
import qualified Luna.Interpreter.Session.TargetHS.TargetHS as TargetHS
import qualified Luna.Interpreter.Session.Var               as Var
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Target.HS.HASTGen                as HASTGen
import qualified Luna.Pass.Target.HS.HSC                    as HSC
import           Luna.Syntax.Enum                           (IDTag)
import           Luna.Syntax.Expr                           (LExpr)
import qualified Luna.Syntax.Name.Hash                      as Hash
import           Luna.System.Session                        as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


processMain :: MemoryManager mm => Session mm (MapForest CallPoint ProfileInfo, MapForest CallPoint Error)
processMain = processMain_ >> (,) <$> Env.getProfileInfos <*> Env.getCompileErrors


processMain_ :: MemoryManager mm => Session mm ()
processMain_ = do
    Env.cleanProfileInfos
    Env.cleanCompileErrors
    TargetHS.reload
    mainPtr  <- Env.getMainPtr
    children <- CallDataPath.addLevel [] mainPtr
    mapM_ processNodeIfNeeded children
    Env.setAllReady True
    Debug.dumpBindings


processNodeIfNeeded :: MemoryManager mm => CallDataPath -> Session mm ()
processNodeIfNeeded callDataPath =
    whenM (Cache.isDirty $ CallDataPath.toCallPointPath callDataPath) $
          processNode callDataPath


processNode :: MemoryManager mm => CallDataPath -> Session mm ()
processNode callDataPath = Profile.debugNode (CallDataPath.toCallPointPath callDataPath) $ do
    arguments <- Traverse.arguments callDataPath
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        keyNames  = map (KeyName . CallDataPath.toCallPointPath) arguments
    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  {} ->
                return ()
            Node.Outputs {} ->
                executeOutputs callDataPath keyNames
            Node.Expr (NodeExpr.StringExpr (StringExpr.Pattern {})) _ _ ->
                executeAssignment callDataPath keyNames
            Node.Expr {}                                                ->
                executeNode       callDataPath keyNames
        else mapM_ processNodeIfNeeded children


executeOutputs :: MemoryManager mm
               => CallDataPath -> [KeyName] -> Session mm ()
executeOutputs callDataPath keyNames = do
    let argsCount  = length $ Traverse.inDataConnections callDataPath
        nodeExpr = NodeExpr.StringExpr $ if argsCount == 1 then StringExpr.Id else StringExpr.Tuple
    when (length callDataPath > 1) $
        execute (init callDataPath) nodeExpr keyNames


executeNode :: MemoryManager mm
            => CallDataPath -> [KeyName] -> Session mm ()
executeNode callDataPath keyNames = case last callDataPath ^. CallData.node of
    Node.Expr nodeExpr _ _ -> execute callDataPath nodeExpr keyNames


executeAssignment :: MemoryManager mm
                  => CallDataPath -> [KeyName] -> Session mm ()
executeAssignment callDataPath [keyName] =
    execute callDataPath (NodeExpr.StringExpr StringExpr.Id) [keyName] -- TODO [PM] : handle Luna's pattern matching


execute :: MemoryManager mm
        => CallDataPath -> NodeExpr -> [KeyName] -> Session mm ()
execute callDataPath nodeExpr keyNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    status       <- Cache.status        callPointPath
    let execFunction = evalFunction nodeExpr callDataPath keyNames
        executeModified = do
            logger debug "processing node"
            rebind <- execFunction True
            Invalidate.markSuccessors callDataPath $ if rebind
                then CacheStatus.Modified
                else CacheStatus.Affected

        executeAffected = do
            logger debug "processing affected node"
            False <- execFunction False
            Invalidate.markSuccessors callDataPath CacheStatus.Affected

    case status of
        CacheStatus.Affected     -> executeAffected
        CacheStatus.Modified     -> executeModified
        CacheStatus.NonCacheable -> executeModified
        CacheStatus.Ready        -> left $ Error.OtherError $(loc) "something went wrong : status = Ready"


evalFunction :: MemoryManager mm
             => NodeExpr -> CallDataPath -> [KeyName] -> Bool -> Session mm Bool
evalFunction nodeExpr callDataPath keyNames recompile = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        keyName = KeyName callPointPath
    compiledNode <- Env.compiledLookup callPointPath
    rebind <- case (compiledNode, recompile) of
        (Just (CompiledNode update _), False) -> do
            Env.updateExpressions update
            logger debug "running compiled code"
            return False
        _ -> do
            let mkArg arg = do
                    str <- Env.keyNameToString arg
                    return $ "((hmapGet " <> str <> " hmap) hmap _time)"
            args <- mapM mkArg keyNames
            let appArgs a = if null a then "" else " $ appNext " <> List.intercalate " $ appNext " (reverse a)
                genNative = List.replaceByMany "#{}" args . List.stripIdx 3 3
                self      = head keyNames
            vt <- varType nodeExpr
            operation <- ("\\(hmap :: HMap) (_time :: Float) -> " <>) <$> case vt of
                List        -> return $ "val [" <> List.intercalate "," args <> "]"
                Id          -> mkArg self
                Native name -> return $ genNative name
                Con    name -> return $ "call" <> appArgs args <> " $ cons_" <> nameHash name
                Var    name -> if null args
                    then left $ Error.OtherError $(loc) "unsupported node type"
                    else (\arg -> "call" <> appArgs (tail args) <> " $ member (Proxy::Proxy " <> show (nameHash name) <> ") " <> arg) <$> mkArg self
                LitInt   name    -> return $ "val (" <> name <> " :: Int)"
                LitFloat name    -> return $ "val (" <> name <> " :: Float)"
                Lit      name    -> return $ "val  " <> name
                Tuple            -> return $ "val (" <> List.intercalate "," args <> ")"
                TimeVar          -> return   "val _time"
                Expression  name -> return   name
            Profile.compileTime callPointPath $ catchEither (left . Error.RunError $(loc) callPointPath) $ do
                keyNameStr <- Env.keyNameToString keyName
                let createKey       = Session.runAssignment keyNameStr $ "unsafePerformIO $ hmapCreateKeyWithWitness $ " <> operation
                    createUpdate    = lift2 $ HEval.interpret $ "\\hmap -> hmapInsert " <> keyNameStr <> " (" <> operation <> ") hmap"
                    createGetValue  = HEval.interpret ("\\hmap mode time -> flip computeValue mode =<< toIOEnv (fromValue ((hmapGet " <> keyNameStr <> " hmap) hmap time))")
                    createKeyUpdate = createKey >> createUpdate
                    valErrHandler (e:: SomeException) = logger warning (show e) >> return Nothing
                (update, rebind) <- Catch.catch ((,False) <$> createUpdate) (\(_ :: SomeException) -> (,True) <$> createKeyUpdate)
                getValue <- lift2 $ flip Catch.catch valErrHandler $
                    Just <$> createGetValue
                Env.compiledInsert callPointPath $ CompiledNode update getValue
                Env.updateExpressions update
                return rebind
    Cache.put callDataPath $ Prelude.error "Executor.hash:Not implemented"
    Value.reportIfVisible callPointPath
    Manager.reportUseMany keyNames
    Manager.reportUse keyName
    return rebind


nameHash :: String -> String
nameHash = Hash.hash


hastExpr :: LExpr IDTag () -> Session mm String
hastExpr expr = do
    result <- Session.runT $ do
        void Parser.init
        runEitherT $ Pass.run1_ HASTGen.passExpr (expr :: LExpr IDTag ())
    cpphsOptions <- Env.getCpphsOptions
    hexpr <- hoistEitherWith (Error.OtherError $(loc) . show) $ fst result
    let code = Text.unpack $ HSC.genExpr hexpr
    unlines . filter (not . null) . lines <$> liftIO (Cpphs.runCpphs cpphsOptions "" code)


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
    | Char.isUpper h                                     = return $ Con name
    | otherwise                                          = return $ Var name
varType (NodeExpr.ASTExpr oldExpr') = do
    oldExpr  <- Var.replaceTimeRefs oldExpr'
    expr     <- mapError $(loc) (convertAST oldExpr)
    Expression . Utils.replace "_time" "(val _time)" <$> hastExpr expr
