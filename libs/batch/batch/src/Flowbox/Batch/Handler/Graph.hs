---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Handler.Graph where

import Control.Monad (forM_)

import           Flowbox.Batch.Batch                (Batch)
import qualified Flowbox.Batch.Handler.Common       as Batch
import qualified Flowbox.Batch.Project.Project      as Project
import           Flowbox.Control.Error              (assertE)
import           Flowbox.Prelude                    hiding (error)
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.Control.Crumb         (Breadcrumbs)
import qualified Luna.DEP.Data.ASTInfo              as ASTInfo
import           Luna.DEP.Graph.Node                (Node)
import qualified Luna.DEP.Graph.Node                as Node
import qualified Luna.DEP.Graph.Node.OutputName     as OutputName
import qualified Luna.DEP.Graph.PropertyMap         as PropertyMap
import           Luna.DEP.Graph.View.EdgeView       (EdgeView (EdgeView))
import           Luna.DEP.Graph.View.GraphView      (GraphView)
import qualified Luna.DEP.Graph.View.GraphView      as GraphView
import           Luna.DEP.Graph.View.PortDescriptor (PortDescriptor)
import qualified Luna.DEP.Lib.Lib                   as Library



loggerIO :: LoggerIO
loggerIO = getLoggerIO $moduleName


nodesGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch GraphView
nodesGraph bc libID projectID = fst <$> Batch.getGraphView bc libID projectID


nodeByID :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Node
nodeByID = Batch.getNode


nodesByIDs :: [Node.ID] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch [(Node.ID, Maybe Node)]
nodesByIDs = Batch.getNodes


nodeEdges :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch [GraphView.LEdge EdgeView]
nodeEdges node bc libID projectID = do
    (graph, _) <- Batch.getGraphView bc libID projectID
    return $ (++) <$> GraphView.inn graph <*> GraphView.out graph $ node -- czy da się lepiej?


addNode :: Node -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Node.ID
addNode node bc libID projectID = do
    (graph, propertyMap) <- Batch.getGraphView bc libID projectID
    astInfo <- Batch.getASTInfo libID projectID
    let astInfo'  = ASTInfo.incID astInfo
        newID     = astInfo' ^. ASTInfo.lastID
        fixedNode = OutputName.fixEmpty node newID
        newGraph  = GraphView.insNode (newID, fixedNode) graph
    Batch.setASTInfo astInfo' libID projectID
    Batch.setGraphView (newGraph, propertyMap) bc libID projectID
    return newID


updateNode :: (Node.ID, Node) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Node.ID
updateNode (nodeID, newNode) bc libID projectID = do
    (nodeID >= 0 || not (Node.isExpr newNode)) `assertE` "Cannot update, wrong node id"
    (graph, propertyMap) <- Batch.getGraphView bc libID projectID
    astInfo <- Batch.getASTInfo libID projectID
    let astInfo'  = ASTInfo.incID astInfo
        newID     = astInfo' ^. ASTInfo.lastID
        fixedNode = OutputName.fixEmpty newNode newID
        newGraph  = GraphView.replaceNode (newID, fixedNode) nodeID graph
        newPropertyMap = PropertyMap.move nodeID newID propertyMap
    Batch.setASTInfo astInfo' libID projectID
    Batch.setGraphView (newGraph, newPropertyMap) bc libID projectID
    return newID


updateNodeInPlace :: (Node.ID, Node) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateNodeInPlace (nodeID, newNode) bc libID projectID = Batch.graphViewOp bc libID projectID $ \graph propertyMap -> do
    (nodeID >= 0 || not (Node.isExpr newNode)) `assertE` "Cannot update, wrong node id"
    let fixedNode = OutputName.fixEmpty newNode nodeID
        newGraph  = GraphView.updateNode (nodeID, fixedNode) graph
    return ((newGraph, propertyMap), ())


removeNodes :: [Node.ID] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
removeNodes nodeIDs bc libID projectID = do
    (graph, propertyMap) <- Batch.getGraphView bc libID projectID
    forM_ nodeIDs (\nodeID -> (nodeID >= 0 && GraphView.gelem nodeID graph) `assertE` ("Wrong 'nodeID' = " ++ show nodeID))
    let newGraph = GraphView.delNodes nodeIDs graph
        newPropertyMap = foldl (flip PropertyMap.delete) propertyMap nodeIDs
    Batch.setGraphView (newGraph, newPropertyMap) bc libID projectID


connect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
        -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
connect srcNodeID srcPort dstNodeID dstPort bc libID projectID = do
    (graph, propertyMap) <- Batch.getGraphView bc libID projectID
    GraphView.gelem srcNodeID graph `assertE` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assertE` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    GraphView.isNotAlreadyConnected graph dstNodeID dstPort `assertE` "Unable to connect: Port is already connected"
    let newGraph = GraphView.insEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    Batch.setGraphView (newGraph, propertyMap) bc libID projectID


disconnect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
           -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID = do
    (graph, propertyMap) <- Batch.getGraphView bc libID projectID
    GraphView.gelem srcNodeID graph `assertE` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graph `assertE` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph
    Batch.setGraphView (newGraph, propertyMap) bc libID projectID
