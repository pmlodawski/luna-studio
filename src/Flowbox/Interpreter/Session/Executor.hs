---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Executor where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List

import           Flowbox.Control.Error
import qualified Flowbox.Data.MapForest                         as MapForest
import qualified Flowbox.Interpreter.Session.Cache              as Cache
import           Flowbox.Interpreter.Session.Data.CallData      (CallData (CallData))
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Data.CallPointPath as CallPointPath
import           Flowbox.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Flowbox.Interpreter.Session.Data.DefPoint      as DefPoint
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Interpreter.Session.Session            as Session
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import qualified Flowbox.Luna.Passes.Analysis.NameResolver      as NameResolver
import           Flowbox.Prelude
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
    let createDataPath (nodeID, node) =
            callDataPath ++ [CallData (CallPoint (defPoint ^. DefPoint.libraryID)
                                                 nodeID)
                                      (defPoint ^. DefPoint.breadcrumbs)
                                      graph
                                      node
                            ]

    mapM_ (processNodeIfNeeded . createDataPath) $ Graph.labNodes graph


processNodeIfNeeded :: CallDataPath -> Session ()
processNodeIfNeeded callDataPath =
    unlessM (Cache.exists $ CallDataPath.toCallPointPath callDataPath)
            (processNode callDataPath)


previous :: CallDataPath -> Session [CallDataPath]
previous []           = return []
previous callDataPath = do
    let graph        = last callDataPath ^. CallData.parentGraph
        predecessors = Graph.prel graph $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID
    List.concat <$> mapM previousCall predecessors
    where
        previousCall (nodeID, node) = case node of
            Node.Inputs -> do print (">>>>>>>>", nodeID, "prev")
                              previous $ init callDataPath
            _        -> do print (">>>>>>>>", nodeID)
                           return [init callDataPath ++ [last callDataPath & CallData.callPoint . CallPoint.nodeID .~ nodeID
                                                                           & CallData.node .~ node
                                                        ]
                                  ]


--next :: CallDataPath -> Session [CallDataPath]
--next []           = return []
--next callDataPath = do
--    let graph = last callDataPath ^. CallData.parentGraph
--        successors = Graph.sucl graph $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID
--    List.concat

processNode :: CallDataPath -> Session ()
processNode callDataPath = do
    predecessors <- previous callDataPath
    print ("*************", length predecessors)
    mapM_ processNodeIfNeeded predecessors
    let callData  = last callDataPath
        libraryID = callData ^. CallData.callPoint . CallPoint.libraryID
        parentBC  = callData ^. CallData.parentBC
        node      = callData ^. CallData.node
    libManager <- Session.getLibManager
    results <- EitherT $ NameResolver.run (node ^. Node.expr) parentBC libraryID libManager
    case results of
        []       -> case node of
                        Node.Inputs  -> return ()
                        Node.Outputs -> return ()
                        Node.Expr {} -> executeNode callDataPath $ map CallDataPath.toCallPointPath predecessors
        [result] -> do logger debug $ "Prepare args " ++ show node
                       processGraph callDataPath (DefPoint (fst result) (snd result))
        _        -> left "Name resolver returned multiple results"


executeNode :: CallDataPath -> [CallPointPath] -> Session ()
executeNode callDataPath predecessors = do
    let node          = last callDataPath ^. CallData.node
        functionName  = node ^. Node.expr
        callPointPath = CallDataPath.toCallPointPath callDataPath
        varName       = CallPointPath.toVarName callPointPath
        --functionType = node ^. Node.cls . Type.repr
        args          = map (CallPointPath.toVarName) predecessors
        function      = "toIO $ extract $ (Operation (" ++ functionName ++ "))"
        argSeparator  = " `call` "
        operation     = List.intercalate argSeparator (function : args)
        expression    = varName ++ " <- " ++ operation
    logger info expression
    --Session.runStmt expression
    Cache.put callPointPath
    logger trace =<< MapForest.draw <$> gets (view Env.cached)

