---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Edge(
    Edge(..),
    noEdges,
) where

import           Flowbox.Prelude                   
import           Flowbox.Luna.Network.Graph.Port   (Port)


data Edge = Edge { dst :: Port
                 } deriving (Show, Read, Ord, Eq)


noEdges :: [Edge]
noEdges = [] 

