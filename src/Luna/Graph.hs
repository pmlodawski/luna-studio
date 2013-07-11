---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Graph(
Graph,
empty
) where

import qualified Data.Graph.Inductive as DG
import Luna.Node(Graph)

empty :: Graph
empty = DG.empty 


