---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import Control.Monad (unless)

import           Flowbox.Batch.Batch                                 (Batch)
import           Flowbox.Batch.Handler.Common                        (readonlyNodeOp)
import qualified Flowbox.Batch.Handler.Common                        as Common
import qualified Flowbox.Batch.Project.Project                       as Project
import           Flowbox.Control.Error                               (assert)
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs             (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Node                        (Node)
import qualified Flowbox.Luna.Data.Graph.Node                        as Node
import           Flowbox.Luna.Data.GraphView.EdgeView                (EdgeView (EdgeView))
import           Flowbox.Luna.Data.GraphView.GraphView               (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView               as GraphView
import           Flowbox.Luna.Data.GraphView.PortDescriptor          (PortDescriptor)
import qualified Flowbox.Luna.Data.PropertyMap                       as PropertyMap
import qualified Flowbox.Luna.Lib.Library                            as Library
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID               as MaxID
import qualified Flowbox.Luna.Passes.General.Luna.Luna               as Luna
import qualified Flowbox.Luna.Passes.Transform.Graph.Node.OutputName as OutputName
import           Flowbox.Prelude                                     hiding (error)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO GraphView
nodesGraph bc libID projectID batch = fst <$> Common.getGraphView bc libID projectID batch


nodeByID :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Node
nodeByID nodeID bc libID projectID = readonlyNodeOp nodeID bc libID projectID (\_ node ->
    return node)


addNode :: Node
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
addNode node bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    ast   <- Common.getAST libID projectID batch
    maxID <- Luna.runIO $ MaxID.run ast
    let newID     = maxID + 1
        fixedNode = OutputName.fixEmpty node newID
        newGraph  = GraphView.insNode (newID, fixedNode) graph
    newBatch <- Common.setGraphView (newGraph, propertyMap) bc libID projectID batch
    unless (node ^. Node.expr == "displayP") $ Common.safeInterpretLibrary libID projectID newBatch
    return (newBatch, newID)


updateNode :: (Node.ID, Node)
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Batch, Node.ID)
updateNode (nodeID, newNode) bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    ast                  <- Common.getAST libID projectID batch
    maxID                <- Luna.runIO $ MaxID.run ast
    let newID     = maxID + 1
        fixedNode = OutputName.fixEmpty newNode newID
        newGraph  = GraphView.replaceNode (newID, fixedNode) nodeID graph
        newPropertyMap = PropertyMap.move nodeID newID propertyMap
    newBatch <- Common.setGraphView (newGraph, newPropertyMap) bc libID projectID batch
    Common.safeInterpretLibrary libID projectID newBatch
    return (newBatch, newID)


updateNodeInPlace :: (Node.ID, Node)
                  -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateNodeInPlace (nodeID, newNode) bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    let fixedNode = OutputName.fixEmpty newNode nodeID
        newGraph  = GraphView.updateNode (nodeID, fixedNode) graph
    Common.setGraphView (newGraph, propertyMap) bc libID projectID batch


removeNode :: Node.ID
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNode nodeID bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    GraphView.gelem nodeID graph `assert` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = GraphView.delNode nodeID graph
        newPropertyMap = PropertyMap.delete nodeID propertyMap
    newBatch <- Common.setGraphView (newGraph, newPropertyMap) bc libID projectID batch
    Common.safeInterpretLibrary libID projectID newBatch
    return newBatch


connect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
connect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    GraphView.gelem srcNodeID graph `assert` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assert` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    GraphView.isNotAlreadyConnected graph dstNodeID dstPort `assert` "Unable to connect: Port is already connected"
    let newGraph = GraphView.insEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    newBatch <- Common.setGraphView (newGraph, propertyMap) bc libID projectID batch
    Common.safeInterpretLibrary libID projectID newBatch
    return newBatch


disconnect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch = do
    (graph, propertyMap) <- Common.getGraphView bc libID projectID batch
    GraphView.gelem srcNodeID graph `assert` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assert` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    newBatch <- Common.setGraphView (newGraph, propertyMap) bc libID projectID batch
    Common.safeInterpretLibrary libID projectID newBatch
    return newBatch
