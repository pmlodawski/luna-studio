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

    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) where

import           Data.Map                                  (Map)

import           Flowbox.Batch.Batch                       (Batch(..))
import           Flowbox.Batch.Handlers.Common             (noresult, readonly, graphOp)
import           Flowbox.Batch.GraphView.EdgeView          (EdgeView(..))
import qualified Flowbox.Batch.GraphView.GraphView       as GraphView
import           Flowbox.Batch.GraphView.GraphView         (GraphView)
import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Control.Error                     ((<?>), ifnot)
import qualified Flowbox.Luna.Lib.Library                as Library
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node(..))


nodesGraph :: Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String GraphView
nodesGraph defID libID projectID = readonly . graphOp defID libID projectID (\_ agraph -> 
    Right (agraph, GraphView.fromGraph agraph))


nodeByID :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Node
nodeByID nodeID defID libID projectID = readonly . graphOp defID libID projectID (\_ agraph -> do
    node <- Graph.lab agraph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    return (agraph, node))


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


connect :: Node.ID -> [Int] -> Node.ID -> [Int] 
        -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
connect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do 
    Graph.gelem srcNodeID agraph `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID agraph `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    (length adstPort <= 1)       `ifnot` "dstPort cannot have more than 1 item."
    -- TODO [PM] : check if port is already connected!
    let newGraph = GraphView.toGraph 
                 $ GraphView.insEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) 
                 $ GraphView.fromGraph agraph
    return (newGraph, ()))


disconnect :: Node.ID -> [Int] -> Node.ID -> [Int] 
           -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
disconnect srcNodeID asrcPort dstNodeID adstPort defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    Graph.gelem srcNodeID agraph `ifnot` ("Wrong 'srcNodeID' = " ++ show srcNodeID)
    Graph.gelem dstNodeID agraph `ifnot` ("Wrong 'dstNodeID' = " ++ show dstNodeID)
    let newGraph = GraphView.toGraph 
                 $ GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView asrcPort adstPort) 
                 $ GraphView.fromGraph agraph
    return (newGraph, ()))


nodeDefaults :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String (Map [Int] DefaultValue)
nodeDefaults nodeID defID libID projectID batch = undefined


setNodeDefault :: [Int] -> DefaultValue
               -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
setNodeDefault dstPort value nodeID defID libID projectID batch = undefined


removeNodeDefault :: [Int]
                  -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
removeNodeDefault dstPort nodeID defID libID projectID batch= undefined