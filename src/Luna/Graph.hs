---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Graph(
Graph
) where

import qualified Data.Graph.Inductive as DG
import qualified Luna.Node as Node
import qualified Luna.Edge as Edge

type Graph = DG.Gr Node.Node Edge.Edge
