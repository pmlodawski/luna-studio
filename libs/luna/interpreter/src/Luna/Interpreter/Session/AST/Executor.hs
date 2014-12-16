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

import           Flowbox.Control.Error                       (catchEither)
import qualified Flowbox.Data.List                           as List
import           Flowbox.Data.MapForest                      (MapForest)
import           Flowbox.Prelude                             as Prelude hiding (children, inside)
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger
import qualified Luna.Graph.Node                             as Node
import qualified Luna.Graph.Node.Expr                        as NodeExpr
import           Luna.Graph.Node.StringExpr                  (StringExpr)
import qualified Luna.Graph.Node.StringExpr                  as StringExpr
import qualified Luna.Interpreter.Session.AST.Traverse       as Traverse
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Invalidate   as Invalidate
import qualified Luna.Interpreter.Session.Cache.Status       as CacheStatus
import qualified Luna.Interpreter.Session.Cache.Value        as Value
import qualified Luna.Interpreter.Session.Data.CallData      as CallData
import           Luna.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import qualified Luna.Interpreter.Session.Data.VarName       as VarName
import qualified Luna.Interpreter.Session.Debug              as Debug
import qualified Luna.Interpreter.Session.Env                as Env
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.Memory.Manager     (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager     as Manager
import           Luna.Interpreter.Session.ProfileInfo        (ProfileInfo)
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import qualified Luna.Interpreter.Session.TargetHS.TargetHS  as TargetHS
import qualified Luna.Pass.Transform.AST.Hash.Hash           as Hash



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


processMain :: MemoryManager mm => Session mm (MapForest CallPoint ProfileInfo)
processMain = processMain_ >> Env.getProfileInfos


processMain_ :: MemoryManager mm => Session mm ()
processMain_ = do
    Env.cleanProfileInfos
    TargetHS.reload
    mainPtr  <- Env.getMainPtr
    children <- CallDataPath.addLevel [] mainPtr
    mapM_ processNodeIfNeeded children
    Env.setAllReady True
    Cache.dumpAll
    Debug.dumpBindings


processNodeIfNeeded :: MemoryManager mm
                    => CallDataPath -> Session mm ()
processNodeIfNeeded callDataPath =
    whenM (Cache.isDirty $ CallDataPath.toCallPointPath callDataPath)
          (processNode callDataPath)


processNode :: MemoryManager mm
            => CallDataPath -> Session mm ()
processNode callDataPath = Env.profile (CallDataPath.toCallPointPath callDataPath) $ do
    arguments <- Traverse.arguments callDataPath
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        mkArg cpp = do
            varName <- Cache.recentVarName cpp
            return (cpp, varName)
    args <- mapM (mkArg . CallDataPath.toCallPointPath) arguments

    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  {} ->
                return ()
            Node.Outputs {} ->
                executeOutputs callDataPath args
            Node.Expr (NodeExpr.StringExpr (StringExpr.Pattern {})) _ _ ->
                executeAssignment callDataPath args
            Node.Expr {}                                                ->
                executeNode       callDataPath args
        else mapM_ processNodeIfNeeded children


executeOutputs :: MemoryManager mm
               => CallDataPath -> [(CallPointPath, VarName)] -> Session mm ()
executeOutputs callDataPath args = do
    let argsCount  = length $ Traverse.inDataConnections callDataPath
        stringExpr = if argsCount == 1 then StringExpr.Id else StringExpr.Tuple
    when (length callDataPath > 1) $
        execute (init callDataPath) stringExpr  args


executeNode :: MemoryManager mm
            => CallDataPath -> [(CallPointPath, VarName)] -> Session mm ()
executeNode callDataPath args = do
    let node       = last callDataPath ^. CallData.node

    stringExpr <- case node of
        Node.Expr (NodeExpr.StringExpr stringExpr) _ _ ->
            return stringExpr
        _                                              ->
            left $ Error.GraphError $(loc) "Wrong node type"
    execute callDataPath stringExpr args


executeAssignment :: MemoryManager mm
                  => CallDataPath -> [(CallPointPath, VarName)] -> Session mm ()
executeAssignment callDataPath [arg] =
    execute callDataPath StringExpr.Id [arg] -- TODO [PM] : handle Luna's pattern matching


execute :: MemoryManager mm
        => CallDataPath -> StringExpr -> [(CallPointPath, VarName)] -> Session mm ()
execute callDataPath stringExpr args = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    status <- Cache.status callPointPath
    case status of
        CacheStatus.Modified     -> evalFunction stringExpr callDataPath args
                                 >> Invalidate.markSuccessors callDataPath CacheStatus.Modified
        CacheStatus.Ready        -> left $ Error.OtherError $(loc) "something went wrong : status = Ready"


data VarType = Lit    String
             | Con    String
             | Var    String
             | Native String
             | Tuple
             | List
             | Id
             deriving Show

varType :: StringExpr -> VarType
varType  StringExpr.Id                 = Id
varType  StringExpr.Tuple              = Tuple
varType  StringExpr.List               = List
varType (StringExpr.Native name      ) = Native name
varType (StringExpr.Expr   []        ) = Prelude.error "varType : empty expression"
varType (StringExpr.Expr   name@(h:_))
    | Maybe.isJust (Read.readMaybe name :: Maybe Char)   = Lit name
    | Maybe.isJust (Read.readMaybe name :: Maybe Int)    = Lit name
    | Maybe.isJust (Read.readMaybe name :: Maybe Double) = Lit name
    | Maybe.isJust (Read.readMaybe name :: Maybe String) = Lit name
    | Char.isUpper h                                     = Con name
    | otherwise                                          = Var name
varType other = Prelude.error $ show other


evalFunction :: MemoryManager mm
             => StringExpr -> CallDataPath -> [(CallPointPath, VarName)] -> Session mm ()
evalFunction stringExpr callDataPath argsData = do
    let argsVarNames = map snd argsData
        callPointPath = CallDataPath.toCallPointPath callDataPath
        varName       = VarName.mk callPointPath
        nameStr       = StringExpr.toString stringExpr
        nameHash      = if nameStr == "Point2" then "Point2" else Hash.hashStr $ StringExpr.toString stringExpr

        mkArg arg = "(Value (Pure "  ++ arg ++ "))"
        args      = map mkArg argsVarNames
        appArgs a = if null a then "" else " $ appNext " ++ List.intercalate " $ appNext " (reverse a)
        genNative = List.replaceByMany "#{}" args . List.stripIdx 3 3

        self      = head argsVarNames
        operation = "toIOEnv $ fromValue $ " ++ case varType stringExpr of
            List        -> "val [" ++ List.intercalate "," args ++ "]"
            Id          -> mkArg self
            Native name -> genNative name
            Con    _    -> "call" ++ appArgs args ++ " $ cons_" ++ nameHash
            Var    _    -> "call" ++ appArgs (tail args) ++ " $ member (Proxy::Proxy " ++ show nameHash ++ ") " ++ mkArg self
            Lit    name -> if Maybe.isJust (Read.readMaybe name :: Maybe Int)
                              then "val (" ++ name ++" :: Int)"
                              else "val (" ++ name ++ ")"
            Tuple       -> "val (" ++ List.intercalate "," args ++ ")"
    catchEither (left . Error.RunError $(loc) callPointPath) $ do

        Session.runAssignment' varName operation

        Cache.put callDataPath argsVarNames varName
        Value.reportIfVisible callPointPath
        Manager.reportUseMany argsData
        Manager.reportUse callPointPath varName
