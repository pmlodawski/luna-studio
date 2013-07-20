---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Lib.Edge(
Edge(..),
EdgeCls(..),
noEdges
) where

import qualified Luna.Lib.Library as Library
	
data EdgeCls = Standard deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
	src :: Library.ID,
	dst :: Library.ID,
	cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)
