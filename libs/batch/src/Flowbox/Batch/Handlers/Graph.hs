---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Graph (
    nodesGraph,
    nodeByID,

    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect,
) where

import           Flowbox.Batch.Batch                      (Batch(..))
import           Flowbox.Batch.Handlers.Common            (noresult, readonly, graphOp, nodeOp)
import           Flowbox.Batch.GraphView.EdgeView         (EdgeView(..))
import qualified Flowbox.Batch.GraphView.GraphView      as GraphView
import           Flowbox.Batch.GraphView.GraphView        (GraphView)
import           Flowbox.Batch.GraphView.PortDescriptor   (PortDescriptor)
import qualified Flowbox.Batch.Project.Project          as Project
import           Flowbox.Control.Error                    (ifnot)
import qualified Flowbox.Luna.Lib.Library               as Library
import qualified Flowbox.Luna.Network.Def.Definition    as Definition
import qualified Flowbox.Luna.Network.Graph.Graph       as Graph
import qualified Flowbox.Luna.Network.Graph.Node        as Node
import           Flowbox.Luna.Network.Graph.Node          (Node(..))



nodesGraph :: Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String GraphView
nodesGraph defID libID projectID = readonly . graphOp defID libID projectID (\_ agraph -> 
    Right (agraph, GraphView.fromGraph agraph))


nodeByID :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Node
nodeByID nodeID defID libID projectID = readonly . nodeOp nodeID defID libID projectID (\_ node -> do
    return (node, node))


addNode :: Node 
        -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String (Batch, Node.ID)
addNode node defID libID projectID = graphOp defID libID projectID (\_ agraph -> 
    Right $ Graph.insNewNode node agraph)


updateNode :: (Node.ID, Node) 
           -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
updateNode (nodeID, node) defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    Graph.gelem nodeID agraph `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = Graph.updateNode (nodeID, node) agraph
    return (newGraph, ()))


removeNode :: Node.ID 
           -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
removeNode nodeID defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    Graph.gelem nodeID agraph `ifnot` ("Wrong 'nodeID' = " ++ show nodeID)
    let newGraph = Graph.delNode nodeID agraph
    return (newGraph, ()))


connect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor 
        -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
connect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do 
    let graphview = GraphView.fromGraph agraph;
    GraphView.gelem srcNodeID graphview `ifnot` ("Unable to connect: Wrong 'srcNodeID' = " ++ show srcNodeID)
    GraphView.gelem dstNodeID graphview `ifnot` ("Unable to connect: Wrong 'dstNodeID' = " ++ show dstNodeID)
    (length adstPort <= 1)              `ifnot` "Unable to connect: dstPort cannot have more than 1 item."
    GraphView.isNotAlreadyConnected graphview dstNodeID adstPort `ifnot` "Unable to connect: Port is already connected"
    let newGraph = GraphView.toGraph 
                 $ GraphView.insEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) graphview
    return (newGraph, ()))


disconnect :: Node.ID -> PortDescriptor -> Node.ID -> PortDescriptor
           -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
disconnect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    Graph.gelem srcNodeID agraph `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID agraph `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = GraphView.toGraph 
                 $ GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) 
                 $ GraphView.fromGraph agraph
    return (newGraph, ()))
