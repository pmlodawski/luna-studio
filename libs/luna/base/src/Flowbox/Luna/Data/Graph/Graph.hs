---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph.Graph(
    module Flowbox.Data.Graph,
    module Flowbox.Luna.Data.Graph.Graph,
) where

import qualified GHC.Exts as Exts

import           Flowbox.Data.Graph           hiding (Edge, Graph)
import qualified Flowbox.Data.Graph           as DG
import qualified Flowbox.Data.List            as List
import           Flowbox.Luna.Data.Graph.Edge (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Edge as Edge
import           Flowbox.Luna.Data.Graph.Node (Node)
import qualified Flowbox.Luna.Data.Graph.Node as Node
import           Flowbox.Luna.Data.Graph.Port (InPort)
import           Flowbox.Prelude              hiding (empty)


type Graph = DG.Graph Node Edge


connect :: Node.ID -> Node.ID -> Edge -> Graph -> Graph
connect srcID dstID edge = insEdge (srcID, dstID, edge)


portMatches :: InPort -> LEdge Edge -> Bool
portMatches _          (_, _, Edge.Monadic) = False
portMatches newDstPort (_, _, Edge.Data _ connectedDstPort) =
    newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph -> Node.ID -> InPort -> Bool
isNotAlreadyConnected graph nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graph nodeID)


sort :: Graph -> [(Node.ID, Node)]
sort graph = DG.topsortStable graph $ Exts.sortWith Node.position' $ DG.labNodes graph


createMonadicEdges :: Graph -> [LEdge Edge]
createMonadicEdges = List.merge mkMonEdge . map fst . sort where
    mkMonEdge a b = (a, b, Edge.Monadic)


addMonadicEdges :: Graph -> Graph
addMonadicEdges graph = DG.insEdges (createMonadicEdges graph) graph


lprelData :: Graph -> Node.ID -> [(Node.ID, Node, Edge)]
lprelData = filter (Edge.isData . view _3) .: DG.lprel
