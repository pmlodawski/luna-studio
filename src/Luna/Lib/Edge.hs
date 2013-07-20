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

import Luna.Lib.Library (LibID)
	
data EdgeCls = Standard deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
	src :: LibID,
	dst :: LibID,
	cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)
