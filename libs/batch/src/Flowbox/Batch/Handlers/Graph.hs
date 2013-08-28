---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Graph (
    nodesGraph,
    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect,
) where

import           Flowbox.Batch.Batch                   (Batch(..))
import           Flowbox.Batch.Handlers.Common         (noresult, readonly, graphOp)
import           Flowbox.Batch.GraphView.EdgeView      (EdgeView(..))
import qualified Flowbox.Batch.GraphView.GraphView   as GraphView
import           Flowbox.Batch.GraphView.GraphView     (GraphView)
import           Flowbox.Control.Error                 (ifnot)
import qualified Flowbox.Luna.Lib.Library            as Library
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
import qualified Flowbox.Luna.Network.Graph.Node     as Node
import           Flowbox.Luna.Network.Graph.Node       (Node(..))


nodesGraph :: Definition.ID -> Library.ID -> Batch -> Either String GraphView
nodesGraph defID libID = readonly . graphOp defID libID (\_ agraph -> 
    Right (agraph, GraphView.fromGraph agraph))


addNode :: Node -> Definition.ID -> Library.ID -> Batch -> Either String (Batch, Node.ID)
addNode node defID libID = graphOp defID libID (\_ agraph -> 
    Right $ Graph.insNewNode node agraph)


updateNode :: (Node.ID, Node) -> Definition.ID -> Library.ID -> Batch -> Either String Batch
updateNode (nodeID, node) defID libID = noresult . graphOp defID libID (\_ agraph -> do
    Graph.gelem nodeID agraph `ifnot` "Wrong `nodeID`"
    let newGraph = Graph.updateNode (nodeID, node) agraph
    return (newGraph, ()))


removeNode :: Node.ID -> Definition.ID -> Library.ID ->  Batch -> Either String Batch
removeNode nodeID defID libID = noresult . graphOp defID libID (\_ agraph -> do
    Graph.gelem nodeID agraph `ifnot` "Wrong `nodeID`"
    let newGraph = Graph.delNode nodeID agraph
    return (newGraph, ()))


connect :: Node.ID -> [Int] -> Node.ID -> [Int] -> Definition.ID -> Library.ID -> Batch -> Either String Batch
connect srcNodeID asrcPort dstNodeID adstPort defID libID = noresult . graphOp defID libID (\_ agraph -> do 
    Graph.gelem srcNodeID agraph `ifnot` "Wrong `srcNodeID`"
    Graph.gelem dstNodeID agraph `ifnot` "Wrong `dstNodeID`"
    (length adstPort <= 1)       `ifnot` "dstPort cannot have more than 1 item."
    let newGraph = GraphView.toGraph 
                 $ GraphView.insEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) 
                 $ GraphView.fromGraph agraph
    return (newGraph, ()))


disconnect :: Node.ID -> [Int] -> Node.ID -> [Int] -> Definition.ID -> Library.ID -> Batch -> Either String Batch
disconnect srcNodeID asrcPort dstNodeID adstPort defID libID = noresult . graphOp defID libID (\_ agraph -> do
    Graph.gelem srcNodeID agraph `ifnot` "Wrong `srcNodeID`"
    Graph.gelem dstNodeID agraph `ifnot` "Wrong `dstNodeID`"
    let newGraph = GraphView.toGraph 
                 $ GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) 
                 $ GraphView.fromGraph agraph
    return (newGraph, ()))
