---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import           Flowbox.Batch.Batch               (Batch)
import           Flowbox.Batch.Handler.Common      (graphOp', nodeOp', noresult, readonly)
import qualified Flowbox.Batch.Project.Project     as Project
import           Flowbox.Control.Error             (ifnot)
import           Flowbox.Luna.Data.AST.Crumb.Crumb (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Edge      (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph     as Graph
import           Flowbox.Luna.Data.Graph.Node      (Node)
import qualified Flowbox.Luna.Data.Graph.Node      as Node
import           Flowbox.Luna.Data.Graph.Port      (InPort, OutPort)
import qualified Flowbox.Luna.Lib.Library          as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Graph
nodesGraph bc libID projectID = readonly . graphOp' bc libID projectID (\_ graph -> do
    return (graph, graph))


nodeByID :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Node
nodeByID nodeID bc libID projectID = readonly . nodeOp' nodeID bc libID projectID (\_ node -> do
    return (node, node))


addNode :: Node
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
addNode node bc libID projectID = graphOp' bc libID projectID (\_ graph ->
    return $ Graph.insNewNode node graph)


updateNode :: (Node.ID, Node)
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateNode (nodeID, node) bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    Graph.gelem nodeID graph `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = Graph.updateNode (nodeID, node) graph
    return (newGraph, ()))


removeNode :: Node.ID
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNode nodeID bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    Graph.gelem nodeID graph `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = Graph.delNode nodeID graph
    return (newGraph, ()))


connect :: Node.ID -> OutPort -> Node.ID -> InPort
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
connect srcNodeID asrcPort dstNodeID adstPort bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    Graph.gelem srcNodeID graph `ifnot` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID graph `ifnot` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = Graph.insEdge (srcNodeID, dstNodeID, Edge asrcPort adstPort) graph
    return (newGraph, ()))


disconnect :: Node.ID -> OutPort -> Node.ID -> InPort
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
disconnect srcNodeID asrcPort dstNodeID adstPort bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    Graph.gelem srcNodeID graph `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID graph `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = Graph.delLEdge (srcNodeID, dstNodeID, Edge asrcPort adstPort) graph
    return (newGraph, ()))
