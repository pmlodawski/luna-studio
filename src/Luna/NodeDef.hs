---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.NodeDef(
NodeDef(..),
noPorts
) where

noPorts :: [String]
noPorts = []

data NodeDef = NodeDef {
	inputs 	:: [String],
	outputs :: [String],
	imports :: [String]
} deriving (Show, Read, Ord, Eq)

