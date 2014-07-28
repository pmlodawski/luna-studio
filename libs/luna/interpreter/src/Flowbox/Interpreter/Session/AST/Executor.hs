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

import qualified Flowbox.Interpreter.Session.AST.Traverse      as Traverse
import qualified Flowbox.Interpreter.Session.Cache.Cache       as Cache
import qualified Flowbox.Interpreter.Session.Cache.Invalidate  as Invalidate
import qualified Flowbox.Interpreter.Session.Cache.Status      as CacheStatus
import qualified Flowbox.Interpreter.Session.Data.CallData     as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath as CallDataPath
import qualified Flowbox.Interpreter.Session.Data.CallPoint    as CallPoint
import           Flowbox.Interpreter.Session.Data.VarName      (VarName)
import qualified Flowbox.Interpreter.Session.Data.VarName      as VarName
import qualified Flowbox.Interpreter.Session.Env               as Env
import qualified Flowbox.Interpreter.Session.Hash              as Hash
import           Flowbox.Interpreter.Session.Session           (Session)
import qualified Flowbox.Interpreter.Session.Session           as Session
import qualified Flowbox.Interpreter.Session.TypeCheck         as TypeCheck
import qualified Flowbox.Luna.Data.Graph.Graph                 as Graph
import qualified Flowbox.Luna.Data.Graph.Node                  as Node
import           Flowbox.Prelude                               hiding (children, inside)
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
    predecessors <- Traverse.previous callDataPath
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        predecessorsPointPaths = map CallDataPath.toCallPointPath predecessors
    predVarNames <- mapM Cache.recentVarName predecessorsPointPaths
    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  -> return ()
            Node.Outputs -> executeOutputs callDataPath predVarNames
            Node.Expr {} -> executeNode    callDataPath predVarNames
        else mapM_ processNodeIfNeeded children


executeOutputs :: CallDataPath -> [VarName] -> Session ()
executeOutputs callDataPath predVarNames = do
    let nodeID        = last callDataPath ^. CallData.callPoint . CallPoint.nodeID
        parentGraph   = last callDataPath ^. CallData.parentGraph
        inDegree      = Graph.indeg parentGraph nodeID
        functionName  = if inDegree == 1 then "id" else '(' : replicate (inDegree-1) ',' ++ ")"
        callPointPath = CallDataPath.toCallPointPath callDataPath
    when (length callDataPath > 1) $ do

        prevVarName  <- Cache.recentVarName callPointPath
        varName <- executeFunction functionName (init callDataPath) predVarNames
        when (varName /= prevVarName) $
            freeVarName varName


executeNode :: CallDataPath -> [VarName] -> Session ()
executeNode callDataPath predVarNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    status       <- Cache.status        callPointPath
    prevVarName  <- Cache.recentVarName callPointPath
    boundVarName <- Cache.dependency predVarNames callPointPath
    let node         = last callDataPath ^. CallData.node
        functionName = node ^. Node.expr
        execFunction = executeFunction functionName callDataPath predVarNames

        executeModified = do
            varName <- execFunction
            when (varName /= prevVarName) $ do
                if boundVarName /= Just varName
                    then do mapM_ freeVarName boundVarName
                            Invalidate.markSuccessors callDataPath CacheStatus.Modified
                    else Invalidate.markSuccessors callDataPath CacheStatus.Affected

        executeAffected = case boundVarName of
            Nothing    -> do
                _ <- execFunction
                Invalidate.markSuccessors callDataPath CacheStatus.Modified
            Just bound -> do
                Cache.setRecentVarName bound callPointPath
                Invalidate.markSuccessors callDataPath CacheStatus.Affected

    case status of
        CacheStatus.Affected     -> executeAffected
        CacheStatus.Modified     -> executeModified
        CacheStatus.NonCacheable -> executeModified
        CacheStatus.Ready    -> left "executeNode (Ready) : somethong went wrong"


executeFunction :: String -> CallDataPath -> [VarName] -> Session VarName
executeFunction funName callDataPath predVarNames = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        tmpVarName    = "_tmp"
    typedFun  <- TypeCheck.function funName predVarNames
    typedArgs <- mapM TypeCheck.variable predVarNames
    let function      = "toIO $ extract $ (Operation (" ++typedFun ++ "))"
        argSeparator  = " `call` "
        operation     = List.intercalate argSeparator (function : typedArgs)
        expression    = tmpVarName ++ " <- " ++ operation
    --logger info expression
    Session.runStmt expression
    hash <- Hash.compute tmpVarName
    let varName = VarName.mk hash callPointPath
    Session.runAssignment varName tmpVarName
    Cache.put callDataPath predVarNames varName
    Cache.dumpAll
    return varName


freeVarName :: VarName -> Session ()
freeVarName varName = Session.runAssignment varName "()"
