---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.AST.Traverse (
  arguments
, inDataConnections
, into
-- , nextLocal
, up
, next
) where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List
import qualified Data.Maybe          as Maybe
import qualified GHC.Exts            as Exts

import           Flowbox.Control.Error
import           Flowbox.Prelude                            hiding (inside, matching, succ)
import           Flowbox.Source.Location                    (loc)
import           Luna.Graph.Edge                            (Edge)
import qualified Luna.Graph.Edge                            as Edge
import qualified Luna.Graph.Graph                           as Graph
import           Luna.Graph.Node                            (Node)
import qualified Luna.Graph.Node                            as Node
import qualified Luna.Interpreter.Session.AST.Inspect       as Inspect
import qualified Luna.Interpreter.Session.Data.CallData     as CallData
import           Luna.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath as CallDataPath
import qualified Luna.Interpreter.Session.Data.CallPoint    as CallPoint
import           Luna.Interpreter.Session.Data.DefPoint     (DefPoint)
import qualified Luna.Interpreter.Session.Error             as Error
import           Luna.Interpreter.Session.Session           (Session)
import qualified Luna.Interpreter.Session.Session           as Session



arguments :: CallDataPath -> Session [CallDataPath]
arguments []           = return []
arguments callDataPath =
    Maybe.catMaybes <$> mapM (globalPredecessor callDataPath)
                             (Exts.sortWith (\edge -> edge ^. _3 ^? Edge.dst)
                                            (inDataConnections callDataPath))


inDataConnections :: CallDataPath -> [(Node.ID, Node, Edge)]
inDataConnections callDataPath = Graph.lprelData graph nodeID where
    graph  = last callDataPath ^. CallData.parentGraph
    nodeID = last callDataPath ^. CallData.callPoint . CallPoint.nodeID


globalPredecessor :: CallDataPath -> (Node.ID, Node, Edge) -> Session (Maybe CallDataPath)
globalPredecessor []           _                    = return Nothing
globalPredecessor callDataPath (nodeID, node, edge) = do
    let upperLevel = init callDataPath
    case node of
        Node.Inputs {} -> if null upperLevel
                            then return Nothing
                            else do found <- matchPredecessor upperLevel edge
                                    globalPredecessor upperLevel found
        _           -> return $ Just $ CallDataPath.updateNode callDataPath node nodeID


matchPredecessor :: CallDataPath -> Edge -> Session (Node.ID, Node, Edge)
matchPredecessor callDataPath edge =
    List.find (flip Edge.match edge . view _3) (inDataConnections callDataPath)
        <??> Error.GraphError $(loc) "Incorrectly connected graph"



into' :: CallDataPath -> Session (Maybe DefPoint)
into' callDataPath = do
    let callData  = last callDataPath
        libraryID = callData ^. CallData.callPoint . CallPoint.libraryID
        parentBC  = callData ^. CallData.parentBC
        node      = callData ^. CallData.node
    case Node.exprStr node of
        Just name -> Inspect.fromName name parentBC libraryID
        Nothing   -> return Nothing


into :: CallDataPath -> Session [CallDataPath]
into callDataPath = do
    mdefPoint <- into' callDataPath
    case mdefPoint of
        Nothing       -> return []
        Just defPoint -> CallDataPath.addLevel callDataPath defPoint


up :: CallDataPath -> CallDataPath
up = init


--next :: CallDataPath -> Session [CallDataPath]
--next []           = return []
--next callDataPath = do
--    let callData  = last callDataPath
--        graph  = callData ^. CallData.parentGraph
--        nodeID = callData ^. CallData.callPoint . CallPoint.nodeID
--        sucl   = Graph.sucl graph nodeID
--    concat <$> mapM (globalSuccessors callDataPath) sucl


--globalSuccessors :: CallDataPath -> (Node.ID, Node)  -> Session [CallDataPath]
--globalSuccessors prevCallDataPath (nodeID, node) = do
--    let callDataPath = CallDataPath.updateNode prevCallDataPath node nodeID
--    inner <- into callDataPath
--    return $ callDataPath : inner

next :: CallDataPath -> Session [CallDataPath]
next []           = return []
next callDataPath = do
    let callData  = last callDataPath
        graph  = callData ^. CallData.parentGraph
        nodeID = callData ^. CallData.callPoint . CallPoint.nodeID
        node   = callData ^. CallData.node
        lsucl  = Graph.lsucl graph nodeID
    if Node.isOutputs node
        then return [init callDataPath]
        else List.nub . concat <$> mapM (globalSuccessors callDataPath) lsucl


globalSuccessors :: CallDataPath -> (Node.ID, Node, Edge)  -> Session [CallDataPath]
globalSuccessors prevCallDataPath (_     , Node.Outputs {}, _   ) = return [init prevCallDataPath]
globalSuccessors prevCallDataPath (nodeID, node           , edge) = do
    let callDataPath = CallDataPath.updateNode prevCallDataPath node nodeID
    mdefPoint <- into' callDataPath
    case mdefPoint of
        Nothing       -> return [callDataPath]
        Just defPoint -> do
            (graph, defID) <- Session.getGraph defPoint
            inputs         <- List.find (Node.isInputs . snd) (Graph.labNodes graph)
                                <??> Error.GraphError $(loc) "cannot find inputs node"
            let succs = List.filter (Edge.match edge . view _3) $ Graph.lsucl graph $ fst inputs
                newCallDataPath = CallDataPath.append callDataPath defPoint defID graph inputs
            concat <$> mapM (globalSuccessors newCallDataPath) succs

