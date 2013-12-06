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
) where

import           Flowbox.Prelude                

import           Flowbox.Data.Graph           hiding (Graph, Edge, empty)
import qualified Flowbox.Data.Graph           as DG
import           Flowbox.Luna.Data.Graph.Edge   (Edge)
import qualified Flowbox.Luna.Data.Graph.Node as Node
import           Flowbox.Luna.Data.Graph.Node   (Node)



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