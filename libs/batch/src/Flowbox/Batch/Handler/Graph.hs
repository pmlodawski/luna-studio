---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import           Flowbox.Batch.Batch               (Batch)
import           Flowbox.Batch.Handler.Common      (graphOp', noresult, readonly)
import qualified Flowbox.Batch.Project.Project     as Project
import           Flowbox.Control.Error             (ifnot)
import           Flowbox.Luna.Data.AST.Crumb.Crumb (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Graph     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph     as Graph
import           Flowbox.Luna.Data.Graph.Node      (Node)
import qualified Flowbox.Luna.Data.Graph.Node      as Node
import qualified Flowbox.Luna.Lib.Library          as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Graph
nodesGraph bc libID projectID = readonly . graphOp' bc libID projectID (\_ graph -> do
    return (graph, graph))



--nodeByID :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Node
--nodeByID nodeID defID libID projectID = readonly . nodeOp nodeID defID libID projectID (\_ node -> do
--    return (node, node))


addNode :: Node
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
addNode node bc libID projectID = graphOp' bc libID projectID (\_ graph ->
    return $ Graph.insNewNode node graph)


updateNode :: (Node.ID, Node)
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateNode (nodeID, node) bc libID projectID = noresult . graphOp' bc libID projectID (\_ graphView -> do
    Graph.gelem nodeID graphView `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraphView = Graph.updateNode (nodeID, node) graphView
    return (newGraphView, ()))


removeNode :: Node.ID
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNode nodeID bc libID projectID = noresult . graphOp' bc libID projectID (\_ graphView -> do
    Graph.gelem nodeID graphView `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraphView = Graph.delNode nodeID graphView
    return (newGraphView, ()))


--connect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
--        -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
--connect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphViewOp defID libID projectID (\_ graphView -> do
--    GraphView.gelem srcNodeID graphView `ifnot` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
--    GraphView.gelem dstNodeID graphView `ifnot` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
--    --(length adstPort <= 1)              `ifnot` "Unable to connect: dstPort cannot have more than 1 item."
--    GraphView.isNotAlreadyConnected graphView dstNodeID adstPort `ifnot` "Unable to connect: Port is already connected"
--    let newGraphView = GraphView.insEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) graphView
--    return (newGraphView, ()))



--disconnect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
--           -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
--disconnect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphViewOp defID libID projectID (\_ graphView -> do
--    Graph.gelem srcNodeID graphView `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
--    Graph.gelem dstNodeID graphView `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
--    let newGraphView = GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) graphView
--    return (newGraphView, ()))
