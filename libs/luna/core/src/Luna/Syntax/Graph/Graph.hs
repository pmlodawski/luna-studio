---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}

module Luna.Syntax.Graph.Graph(
    module Flowbox.Data.Graph,
    module Luna.Syntax.Graph.Graph,
) where

import qualified Data.List as List

import           Flowbox.Data.Graph     hiding (Edge, Graph)
import qualified Flowbox.Data.Graph     as DG
import qualified Flowbox.Data.List      as List
import           Flowbox.Prelude        hiding (empty)
import           Luna.Syntax.Graph.Edge (Edge)
import qualified Luna.Syntax.Graph.Edge as Edge
import           Luna.Syntax.Graph.Node (Node)
import qualified Luna.Syntax.Graph.Node as Node
import           Luna.Syntax.Graph.Port (DstPort)



type Graph a e = DG.Graph (Node a e) Edge


connect :: Node.ID -> Node.ID -> Edge -> Graph a e -> Graph a e
connect srcID dstID edge = insEdge (srcID, dstID, edge)


portMatches :: DstPort -> LEdge Edge -> Bool
portMatches _          (_, _, Edge.Monadic) = False
portMatches newDstPort (_, _, Edge.Data _ connectedDstPort) =
    newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph a e -> Node.ID -> DstPort -> Bool
isNotAlreadyConnected graph nodeID dstPort = not connected where
    connected = any (portMatches dstPort) (inn graph nodeID)


sort :: (Eq a, Eq e) => Graph a e -> [(Node.ID, Node a e)]
sort graph = DG.topsortStable graph $ List.sortBy compareNodes $ DG.labNodes graph where
    compareNodes :: (Node.ID, Node a e) -> (Node.ID, Node a e) -> Ordering
    compareNodes (_, aNode) (_, bNode)
        |      Node.isInputs  aNode  && not (Node.isInputs  bNode) = LT
        | not (Node.isInputs  aNode) &&      Node.isInputs  bNode  = GT
        |      Node.isOutputs aNode  && not (Node.isOutputs bNode) = GT
        | not (Node.isOutputs aNode) &&      Node.isOutputs bNode  = LT
        | otherwise = compare (aNode ^. Node.pos) (bNode ^. Node.pos)


createMonadicEdges :: (Eq a, Eq e) => Graph a e -> [LEdge Edge]
createMonadicEdges = List.merge mkMonEdge . map fst . sort where
    mkMonEdge a b = (a, b, Edge.Monadic)


addMonadicEdges :: (Eq a, Eq e) => Graph a e -> Graph a e
addMonadicEdges graph = DG.insEdges (createMonadicEdges graph) graph


lprelData :: Graph a e -> Node.ID -> [(Node.ID, Node a e, Edge)]
lprelData = filter (Edge.isData . view _3) .: DG.lprel


lsuclData :: Graph a e -> Node.ID -> [(Node.ID, Node a e, Edge)]
lsuclData = filter (Edge.isData . view _3) .: DG.lsucl


inputsNode :: DG.Graph (Node a e) l -> Maybe (Node.ID, Node a e)
inputsNode graph = (Node.inputsID,) <$> DG.lab graph Node.inputsID


inDataDeg :: Graph a e -> Node.ID -> Int
inDataDeg = length .: lprelData


outDataDeg :: Graph a e -> Node.ID -> Int
outDataDeg = length .: lsuclData
