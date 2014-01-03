---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import           Flowbox.Batch.Batch                        (Batch)
import           Flowbox.Batch.Handler.Common               (graphViewOp, noresult, readonly, readonlyNodeOp)
import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Control.Error                      (assert)
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs    (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Node               (Node)
import qualified Flowbox.Luna.Data.Graph.Node               as Node
import           Flowbox.Luna.Data.GraphView.EdgeView       (EdgeView (EdgeView))
import           Flowbox.Luna.Data.GraphView.GraphView      (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView      as GraphView
import           Flowbox.Luna.Data.GraphView.PortDescriptor (PortDescriptor)
import qualified Flowbox.Luna.Data.PropertyMap              as PropertyMap
import qualified Flowbox.Luna.Lib.Library                   as Library
import           Flowbox.Prelude                            hiding (error)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO GraphView
nodesGraph bc libID projectID = readonly . graphViewOp bc libID projectID (\_ graph propertyMap _ -> do
    return ((graph, propertyMap), graph))


nodeByID :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Node
nodeByID nodeID bc libID projectID = readonlyNodeOp nodeID bc libID projectID (\_ node -> do
    return node)


addNode :: Node
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
addNode node bc libID projectID = graphViewOp bc libID projectID (\_ graph propertyMap maxID -> do
    let newID = maxID + 1
    return ((GraphView.insNode (newID, node) graph, propertyMap), newID))


updateNode :: (Node.ID, Node)
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
updateNode (nodeID, newNode) bc libID projectID = graphViewOp bc libID projectID (\_ graph propertyMap maxID -> do
    let newID    = maxID + 1
        newGraph = GraphView.replaceNode (newID, newNode) nodeID graph
        newPropertyMap = case PropertyMap.lookup nodeID propertyMap of 
            Nothing -> propertyMap
            Just k  -> PropertyMap.insert newID k $ PropertyMap.delete nodeID propertyMap
    return ((newGraph, newPropertyMap), newID))


removeNode :: Node.ID
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNode nodeID bc libID projectID = noresult . graphViewOp bc libID projectID (\_ graph propertyMap _ -> do
    GraphView.gelem nodeID graph `assert` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = GraphView.delNode nodeID graph
        newPropertyMap = PropertyMap.delete nodeID propertyMap
    return ((newGraph, newPropertyMap), ()))


connect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
connect srcNodeID srcPort dstNodeID dstPort bc libID projectID = noresult . graphViewOp bc libID projectID (\_ graph propertyMap _ -> do
    GraphView.gelem srcNodeID graph `assert` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assert` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    GraphView.isNotAlreadyConnected graph dstNodeID dstPort `assert` "Unable to connect: Port is already connected"
    let newGraph = GraphView.insEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    return ((newGraph, propertyMap), ()))


disconnect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID = noresult . graphViewOp bc libID projectID (\_ graph propertyMap _ -> do
    GraphView.gelem srcNodeID graph `assert` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assert` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    return ((newGraph, propertyMap), ()))
