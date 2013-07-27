---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Graph(
    module Luna.Data.Graph,
    Graph(..)
) where

import           Luna.Network.Graph.Edge   (Edge)
import           Luna.Network.Graph.Node   (Node)

import           Luna.Data.Graph         hiding(Graph, Edge)
import qualified Luna.Data.Graph         as DG


type Graph = DG.Graph Node Edge