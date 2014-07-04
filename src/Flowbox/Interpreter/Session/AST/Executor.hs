---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Executor where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List

import qualified Flowbox.Data.MapForest                         as MapForest
import qualified Flowbox.Interpreter.Session.AST.Traverse       as Traverse
import qualified Flowbox.Interpreter.Session.Cache              as Cache
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Data.CallPointPath as CallPointPath
import           Flowbox.Interpreter.Session.Data.DefPoint      (DefPoint)
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Interpreter.Session.Session            as Session
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Prelude                                hiding (inside)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Executor"


--invalidate :: CallPath -> Session ()
--invalidate callPath = do
--    modify (Env.cached %~ MapForest.delete callPath)
--    let varName = CallPath.toVarName callPath
--    logger debug $ "Invalidating " ++ show varName
--    Session.runStmt (varName ++ " <- return ()")
    --mapM_ (invalidate graph) $ Node.suc graph nodeID


findMain :: Session DefPoint
findMain = gets $ view Env.mainPtr


processMain :: Session ()
processMain = do
    mainPtr <- gets $ view Env.mainPtr
    processGraph [] mainPtr


processGraph :: CallDataPath -> DefPoint -> Session ()
processGraph callDataPath defPoint = do
    graph <- Session.getGraph defPoint
    print defPoint
    print graph
    let createDataPath = CallDataPath.append callDataPath defPoint graph
    mapM_ (processNodeIfNeeded . createDataPath) $ Graph.labNodes graph


processNodeIfNeeded :: CallDataPath -> Session ()
processNodeIfNeeded callDataPath =
    unlessM (Cache.exists $ CallDataPath.toCallPointPath callDataPath)
            (processNode callDataPath)


processNode :: CallDataPath -> Session ()
processNode callDataPath = do
    predecessors <- Traverse.previous callDataPath
    mapM_ processNodeIfNeeded predecessors
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        predecessorsPointPaths = map CallDataPath.toCallPointPath predecessors
    inside <- Traverse.into callDataPath
    case inside of
        Nothing       -> case node of
            Node.Inputs  -> return ()
            Node.Outputs -> executeOutputs callDataPath predecessorsPointPaths
            Node.Expr {} -> executeNode    callDataPath predecessorsPointPaths
        Just defPoint -> processGraph callDataPath defPoint


executeOutputs :: CallDataPath -> [CallPointPath] -> Session ()
executeOutputs callDataPath predecessors = do
    let nodeID        = last callDataPath ^. CallData.callPoint . CallPoint.nodeID
        parentGraph   = last callDataPath ^. CallData.parentGraph
        inDegree      = Graph.indeg parentGraph nodeID
        functionName  = if inDegree == 1 then "id" else '(' : replicate (inDegree-1) ',' ++ ")"
    if length callDataPath > 1
        then executeFunction functionName (CallDataPath.toCallPointPath $ init callDataPath) predecessors
        else return () -- main don't need to return anything


executeNode :: CallDataPath -> [CallPointPath] -> Session ()
executeNode callDataPath predecessors = do
    let node          = last callDataPath ^. CallData.node
        functionName  = node ^. Node.expr
    executeFunction functionName (CallDataPath.toCallPointPath callDataPath) predecessors


executeFunction :: String -> CallPointPath -> [CallPointPath] -> Session ()
executeFunction functionName callPointPath predecessors = do
    let varName       = CallPointPath.toVarName callPointPath
        --functionType = node ^. Node.cls . Type.repr
        args          = map CallPointPath.toVarName predecessors
        function      = "toIO $ extract $ (Operation (" ++ functionName ++ "))"
        argSeparator  = " `call` "
        operation     = List.intercalate argSeparator (function : args)
        expression    = varName ++ " <- " ++ operation
    logger info expression
    --Session.runStmt expression
    Cache.put callPointPath
    logger trace =<< MapForest.draw <$> gets (view Env.cached)
