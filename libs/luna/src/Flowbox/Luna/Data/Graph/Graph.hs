---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph.Graph(
    module Flowbox.Data.Graph,
    Graph,
    empty,
    connect,
    inputsID,
    outputID,
    make,

    portMatches,
    isNotAlreadyConnected
) where

import Flowbox.Prelude hiding (empty)

import           Flowbox.Data.Graph           hiding (Edge, Graph, empty)
import qualified Flowbox.Data.Graph           as DG
import           Flowbox.Luna.Data.Graph.Edge (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Node (Node)
import qualified Flowbox.Luna.Data.Graph.Node as Node
import           Flowbox.Luna.Data.Graph.Port (InPort)


type Graph = DG.Graph Node Edge


empty :: Graph
empty = DG.empty


connect :: Node.ID -> Node.ID -> Edge -> Graph -> Graph
connect srcID dstID edge = insEdge (srcID, dstID, edge)


inputsID :: Node.ID
inputsID = 0


outputID :: Node.ID
outputID = 1


make :: Graph
make = insNode (inputsID, Node.mkInputs)
     $ insNode (outputID, Node.mkOutputs)
     $ empty



portMatches :: InPort -> LEdge Edge -> Bool
portMatches newDstPort (_, _, Edge _ connectedDstPort) = newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph -> Node.ID -> InPort -> Bool
isNotAlreadyConnected graph nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graph nodeID)

