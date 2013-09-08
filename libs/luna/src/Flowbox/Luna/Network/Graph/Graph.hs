---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Graph(
    module Flowbox.Luna.Data.Graph,
    Graph,
    empty,

    connect,
    make,
) where

import           Flowbox.Prelude                   

import           Flowbox.Luna.Network.Graph.Edge   (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node as Node
import           Flowbox.Luna.Network.Graph.Node   (Node(..))

import           Flowbox.Luna.Data.Graph         hiding (Graph, Edge, empty)
import qualified Flowbox.Luna.Data.Graph         as DG


type Graph = DG.Graph Node Edge

empty :: Graph
empty = DG.empty


connect :: Graph -> Node.ID -> Node.ID -> Int -> Graph
connect graph srcID dstID dstPort = insEdge (srcID, dstID, Edge dstPort) graph


make :: Graph
make = insNode (0, Node.mkInputs)
	 $ insNode (1, Node.mkOutputs)
	 $ empty