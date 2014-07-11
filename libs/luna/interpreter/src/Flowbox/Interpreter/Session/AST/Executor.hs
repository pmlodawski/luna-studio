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
import qualified Flowbox.Interpreter.Session.Cache.Cache        as Cache
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Data.CallPointPath as CallPointPath
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Interpreter.Session.Session            as Session
import qualified Flowbox.Interpreter.Session.TypeCheck          as TypeCheck
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Prelude                                hiding (children, inside)
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
    unlessM (Cache.exists $ CallDataPath.toCallPointPath callDataPath)
            (processNode callDataPath)


processNode :: CallDataPath -> Session ()
processNode callDataPath = do
    predecessors <- Traverse.previous callDataPath
    mapM_ processNodeIfNeeded predecessors
    let callData  = last callDataPath
        node      = callData ^. CallData.node
        predecessorsPointPaths = map CallDataPath.toCallPointPath predecessors
    children <- Traverse.into callDataPath
    if null children
        then case node of
            Node.Inputs  -> return ()
            Node.Outputs -> executeOutputs callDataPath predecessorsPointPaths
            Node.Expr {} -> executeNode    callDataPath predecessorsPointPaths
        else mapM_ processNodeIfNeeded children


executeOutputs :: CallDataPath -> [CallPointPath] -> Session ()
executeOutputs callDataPath predecessors = do
    let nodeID        = last callDataPath ^. CallData.callPoint . CallPoint.nodeID
        parentGraph   = last callDataPath ^. CallData.parentGraph
        inDegree      = Graph.indeg parentGraph nodeID
        functionName  = if inDegree == 1 then "id" else '(' : replicate (inDegree-1) ',' ++ ")"
    if length callDataPath > 1
        then executeFunction functionName (init callDataPath) predecessors
        else return () -- main don't need to return anything


executeNode :: CallDataPath -> [CallPointPath] -> Session ()
executeNode callDataPath predecessors = do
    let node          = last callDataPath ^. CallData.node
        functionName  = node ^. Node.expr
    executeFunction functionName (callDataPath) predecessors


executeFunction :: String -> CallDataPath -> [CallPointPath] -> Session ()
executeFunction funName callDataPath predecessors = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        varName       = CallPointPath.toVarName callPointPath
        args          = map CallPointPath.toVarName predecessors
    typedFun  <- TypeCheck.function funName args
    typedArgs <- mapM TypeCheck.variable args
    let function      = "toIO $ extract $ (Operation (" ++typedFun ++ "))"
        argSeparator  = " `call` "
        operation     = List.intercalate argSeparator (function : typedArgs)
        expression    = varName ++ " <- " ++ operation
    --logger info expression
    Session.runStmt expression
    Cache.put callDataPath
    logger trace =<< MapForest.draw <$> gets (view Env.cached)


