---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Graph.Graph(
    module Flowbox.Data.Graph,
    module Luna.Graph.Graph,
) where

import qualified Data.List as List

import           Flowbox.Data.Graph hiding (Edge, Graph)
import qualified Flowbox.Data.Graph as DG
import qualified Flowbox.Data.List  as List
import           Flowbox.Prelude    hiding (empty)
import           Luna.Graph.Edge    (Edge)
import qualified Luna.Graph.Edge    as Edge
import           Luna.Graph.Node    (Node)
import qualified Luna.Graph.Node    as Node
import           Luna.Graph.Port    (Port)


type Graph = DG.Graph Node Edge


connect :: Node.ID -> Node.ID -> Edge -> Graph -> Graph
connect srcID dstID edge = insEdge (srcID, dstID, edge)


portMatches :: Port -> LEdge Edge -> Bool
portMatches _          (_, _, Edge.Monadic) = False
portMatches newDstPort (_, _, Edge.Data _ connectedDstPort) =
    newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph -> Node.ID -> Port -> Bool
isNotAlreadyConnected graph nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graph nodeID)


sort :: Graph -> [(Node.ID, Node)]
sort graph = DG.topsortStable graph $ List.sortBy compareNodes $ DG.labNodes graph where
    compareNodes :: (Node.ID, Node) -> (Node.ID, Node) -> Ordering
    compareNodes (_, aNode) (_, bNode)
        |      Node.isInputs  aNode  && not (Node.isInputs  bNode) = LT
        | not (Node.isInputs  aNode) &&      Node.isInputs  bNode  = GT
        |      Node.isOutputs aNode  && not (Node.isOutputs bNode) = GT
        | not (Node.isOutputs aNode) &&      Node.isOutputs bNode  = LT
        | otherwise = compare (aNode ^. Node.pos) (bNode ^. Node.pos)


createMonadicEdges :: Graph -> [LEdge Edge]
createMonadicEdges = List.merge mkMonEdge . map fst . sort where
    mkMonEdge a b = (a, b, Edge.Monadic)


addMonadicEdges :: Graph -> Graph
addMonadicEdges graph = DG.insEdges (createMonadicEdges graph) graph


lprelData :: Graph -> Node.ID -> [(Node.ID, Node, Edge)]
lprelData = filter (Edge.isData . view _3) .: DG.lprel


lsuclData :: Graph -> Node.ID -> [(Node.ID, Node, Edge)]
lsuclData = filter (Edge.isData . view _3) .: DG.lsucl


inputsNode :: DG.Graph Node a -> Maybe (Node.ID, Node)
inputsNode = List.find (Node.isInputs . snd) . DG.labNodes
