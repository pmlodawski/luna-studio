---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Executor where

import           Control.Monad.State        hiding (mapM, mapM_)
import           Control.Monad.Trans.Either
import qualified Data.List                  as List

import qualified Flowbox.Interpreter.Session.AST.Traverse          as Traverse
import qualified Flowbox.Interpreter.Session.Cache.Cache           as Cache
import qualified Flowbox.Interpreter.Session.Cache.Invalidate      as Invalidate
import qualified Flowbox.Interpreter.Session.Cache.Status          as CacheStatus
import qualified Flowbox.Interpreter.Session.Data.CallData         as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath     (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath     as CallDataPath
import           Flowbox.Interpreter.Session.Data.VarName          (VarName)
import qualified Flowbox.Interpreter.Session.Data.VarName          as VarName
import qualified Flowbox.Interpreter.Session.Env                   as Env
import qualified Flowbox.Interpreter.Session.Hash                  as Hash
import           Flowbox.Interpreter.Session.Session               (Session)
import qualified Flowbox.Interpreter.Session.Session               as Session
import qualified Flowbox.Interpreter.Session.TypeCheck             as TypeCheck
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser as GraphParser
import           Flowbox.Prelude                                   hiding (children, inside)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Executor"


processMain :: Session ()
processMain = do
    mainPtr <- gets $ view Env.mainPtr
    children <- CallDataPath.addLevel [] mainPtr
    mapM_ processNodeIfNeeded children


processNodeIfNeeded :: CallDataPath -> Session ()
processNodeIfNeeded callDataPath =
    whenM (Cache.isDirty $ CallDataPath.toCallPointPath callDataPath)
          (processNode callDataPath)


processNode :: CallDataPath -> Session ()
processNode callDataPath = do
    arguments <- Traverse.arguments callDataPath
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        argumentsPointPaths = map CallDataPath.toCallPointPath arguments
    argsVarNames <- mapM Cache.recentVarName argumentsPointPaths
    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  {} -> return ()
            Node.Outputs {} -> executeOutputs callDataPath argsVarNames
            Node.Expr    {} -> executeNode    callDataPath argsVarNames
        else mapM_ processNodeIfNeeded children


executeOutputs :: CallDataPath -> [VarName] -> Session ()
executeOutputs callDataPath argsVarNames = do
    let argsCount     = length $ Traverse.inDataConnections callDataPath
        functionName  = if argsCount == 1 then "id" else '(' : replicate (argsCount-1) ',' ++ ")"
    when (length callDataPath > 1) $
        execute (init callDataPath) functionName  argsVarNames


executeNode :: CallDataPath -> [VarName] -> Session ()
executeNode callDataPath argsVarNames = do
    let node         = last callDataPath ^. CallData.node
        functionName = node ^. Node.expr
    execute callDataPath functionName argsVarNames


execute :: CallDataPath -> String -> [VarName] -> Session ()
execute callDataPath functionName argsVarNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    status       <- Cache.status        callPointPath
    prevVarName  <- Cache.recentVarName callPointPath
    boundVarName <- Cache.dependency argsVarNames callPointPath
    let execFunction = evalFunction functionName callDataPath argsVarNames

        executeModified = do
            varName <- execFunction
            if varName /= prevVarName
                then if boundVarName /= Just varName
                        then do logger debug "processing modified node - result value differs"
                                mapM_ freeVarName boundVarName
                                Invalidate.markSuccessors callDataPath CacheStatus.Modified

                        else do logger debug "processing modified node - result value differs but is cached"
                                Invalidate.markSuccessors callDataPath CacheStatus.Affected
                else logger debug "processing modified node - result value is same"

        executeAffected = case boundVarName of
            Nothing    -> do
                logger debug "processing affected node - result never bound"
                _ <- execFunction
                Invalidate.markSuccessors callDataPath CacheStatus.Modified
            Just bound -> do
                logger debug "processing affected node - result rebound"
                Cache.setRecentVarName bound callPointPath
                Invalidate.markSuccessors callDataPath CacheStatus.Affected

    case status of
        CacheStatus.Affected     -> executeAffected
        CacheStatus.Modified     -> executeModified
        CacheStatus.NonCacheable -> executeModified
        CacheStatus.Ready        -> left "executeNode (Ready) : something went wrong"


evalFunction :: String -> CallDataPath -> [VarName] -> Session VarName
evalFunction funName callDataPath argsVarNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        tmpVarName    = "_tmp"
        funStr = if GraphParser.isOperator funName then "(" ++ funName ++ ")" else funName
    typedFun  <- TypeCheck.function funStr argsVarNames
    typedArgs <- mapM TypeCheck.variable argsVarNames
    let function      = "toIO $ extract $ (Operation (" ++typedFun ++ "))"
        argSeparator  = " `call` "
        operation     = List.intercalate argSeparator (function : typedArgs)
        expression    = tmpVarName ++ " <- " ++ operation
    Session.runStmt expression
    hash <- Hash.compute tmpVarName
    let varName = VarName.mk hash callPointPath
    Session.runAssignment varName tmpVarName
    Cache.put callDataPath argsVarNames varName
    return varName


freeVarName :: VarName -> Session ()
freeVarName varName = Session.runAssignment varName "()"
