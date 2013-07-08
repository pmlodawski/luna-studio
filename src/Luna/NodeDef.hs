---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.NodeDef(
NodeDef(..)
) where

import qualified Data.Graph.Inductive as DG
import qualified Luna.Node as Node
import qualified Luna.Edge as Edge

type Graph = DG.Gr Node.Node Edge.Edge

data NodeDef = NodeDef {
	name :: String,
	graph :: Graph,
	inputs :: [String],
	outputs :: [String]
} deriving (Show)
