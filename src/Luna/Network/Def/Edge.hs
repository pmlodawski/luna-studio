---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Def.Edge(
Edge(..),
EdgeCls(..),
noEdges
) where

import qualified Luna.Network.Def.NodeDef as NodeDef
	
data EdgeCls = Standard deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
	src :: NodeDef.ID,
	dst :: NodeDef.ID,
	cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)
