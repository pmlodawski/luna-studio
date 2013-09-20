---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Graph(
    module Flowbox.Data.Graph,
    Graph,
    empty,

    connect,
    make,
) where

import           Flowbox.Prelude                   

import           Flowbox.Luna.Network.Graph.Edge   (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node as Node
import           Flowbox.Luna.Network.Graph.Node   (Node(..))

import           Flowbox.Data.Graph              hiding (Graph, Edge, empty)
import qualified Flowbox.Data.Graph              as DG


type Graph = DG.Graph Node Edge

empty :: Graph
empty = DG.empty


connect :: Graph -> Node.ID -> Node.ID -> Edge -> Graph
connect graph srcID dstID edge = insEdge (srcID, dstID, edge) graph


make :: Graph
make = insNode (0, Node.mkInputs)
	 $ insNode (1, Node.mkOutputs)
	 $ empty