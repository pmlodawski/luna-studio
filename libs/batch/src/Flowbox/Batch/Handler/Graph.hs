---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import           Flowbox.Batch.Batch               (Batch)
import           Flowbox.Batch.Handler.Common      (graphOp, noresult, readonly, readonlyNodeOp)
import qualified Flowbox.Batch.Project.Project     as Project
import           Flowbox.Control.Error             (ifnot)
import           Flowbox.Luna.Data.AST.Crumb.Crumb (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Edge      (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph     as Graph
import           Flowbox.Luna.Data.Graph.Node      (Node)
import qualified Flowbox.Luna.Data.Graph.Node      as Node
import           Flowbox.Luna.Data.Graph.Port      (InPort, OutPort)
import qualified Flowbox.Luna.Data.PropertyMap     as PropertyMap
import qualified Flowbox.Luna.Lib.Library          as Library
import           Flowbox.Prelude                   hiding (error)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Graph
nodesGraph bc libID projectID = readonly . graphOp bc libID projectID (\_ graph propertyMap _ -> do
    return ((graph, propertyMap), graph))


nodeByID :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Node
nodeByID nodeID bc libID projectID = readonlyNodeOp nodeID bc libID projectID (\_ node -> do
    return node)


addNode :: Node
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
addNode node bc libID projectID = graphOp bc libID projectID (\_ graph propertyMap maxID -> do
    let newID = maxID + 1
    return ((Graph.insNode (newID, node) graph, propertyMap), newID))


removeNode :: Node.ID
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNode nodeID bc libID projectID = noresult . graphOp bc libID projectID (\_ graph propertyMap _ -> do
    Graph.gelem nodeID graph `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = Graph.delNode nodeID graph
        newPropertyMap = PropertyMap.delete nodeID propertyMap
    return ((newGraph, newPropertyMap), ()))


connect :: Node.ID -> OutPort -> Node.ID -> InPort
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
connect srcNodeID srcPort dstNodeID dstPort bc libID projectID = noresult . graphOp bc libID projectID (\_ graph propertyMap _ -> do
    Graph.gelem srcNodeID graph `ifnot` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID graph `ifnot` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    Graph.isNotAlreadyConnected graph dstNodeID dstPort `ifnot` "Unable to connect: Port is already connected"
    let newGraph = Graph.insEdge (srcNodeID, dstNodeID, Edge srcPort dstPort) graph
    return ((newGraph, propertyMap), ()))


disconnect :: Node.ID -> OutPort -> Node.ID -> InPort
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID = noresult . graphOp bc libID projectID (\_ graph propertyMap _ -> do
    Graph.gelem srcNodeID graph `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID graph `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = Graph.delLEdge (srcNodeID, dstNodeID, Edge srcPort dstPort) graph
    return ((newGraph, propertyMap), ()))
