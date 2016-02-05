---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}

module Luna.Syntax.Graph.Graph (
    module Flowbox.Data.Graph,
    module Luna.Syntax.Graph.Graph,
) where

import qualified Data.List              as List

import           Flowbox.Data.Graph     hiding (Edge, Graph)
import qualified Flowbox.Data.Graph     as DG
import qualified Flowbox.Data.List      as List
import           Flowbox.Prelude        hiding (empty)
import           Luna.Syntax.Graph.Edge (Edge)
import qualified Luna.Syntax.Graph.Edge as Edge
import           Luna.Syntax.Graph.Node (Node)
import qualified Luna.Syntax.Graph.Node as Node
import           Luna.Syntax.Graph.Port (DstPort)



type Graph a v = DG.Graph (Node a v) Edge


connect :: Node.ID -> Node.ID -> Edge -> Graph a v -> Graph a v
connect srcID dstID vdge = insEdge (srcID, dstID, vdge)


portMatches :: DstPort -> LEdge Edge -> Bool
portMatches _          (_, _, Edge.Monadic) = False
portMatches newDstPort (_, _, Edge.Data _ connectedDstPort) =
    newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph a v -> Node.ID -> DstPort -> Bool
isNotAlreadyConnected graph nodeID dstPort = not connected where
    connected = any (portMatches dstPort) (inn graph nodeID)


sort :: (Eq a, Eq v) => Graph a v -> [(Node.ID, Node a v)]
sort graph = DG.topsortStable graph $ List.sortBy compareNodes $ DG.labNodes graph where
    compareNodes :: (Node.ID, Node a v) -> (Node.ID, Node a v) -> Ordering
    compareNodes (_, aNode) (_, bNode)
        |      Node.isInputs  aNode  && not (Node.isInputs  bNode) = LT
        | not (Node.isInputs  aNode) &&      Node.isInputs  bNode  = GT
        |      Node.isOutputs aNode  && not (Node.isOutputs bNode) = GT
        | not (Node.isOutputs aNode) &&      Node.isOutputs bNode  = LT
        | otherwise = compare (aNode ^. Node.pos) (bNode ^. Node.pos)


createMonadicEdges :: (Eq a, Eq v) => Graph a v -> [LEdge Edge]
createMonadicEdges = List.merge mkMonEdge . map fst . sort where
    mkMonEdge a b = (a, b, Edge.Monadic)


addMonadicEdges :: (Eq a, Eq v) => Graph a v -> Graph a v
addMonadicEdges graph = DG.insEdges (createMonadicEdges graph) graph


lprelData :: Graph a v -> Node.ID -> [(Node.ID, Node a v, Edge)]
lprelData = filter (Edge.isData . view _3) .: DG.lprel


lsuclData :: Graph a v -> Node.ID -> [(Node.ID, Node a v, Edge)]
lsuclData = filter (Edge.isData . view _3) .: DG.lsucl


inputsNode :: DG.Graph (Node a v) l -> Maybe (Node.ID, Node a v)
inputsNode graph = (Node.inputsID,) <$> DG.lab graph Node.inputsID


inDataDeg :: Graph a v -> Node.ID -> Int
inDataDeg = length .: lprelData


outDataDeg :: Graph a v -> Node.ID -> Int
outDataDeg = length .: lsuclData


toStringNodes :: (Show a, Show v) => Graph a v -> Graph a v
toStringNodes = DG.nmap Node.toStringNode
