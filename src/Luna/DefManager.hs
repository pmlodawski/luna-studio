---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
empty,
insert
) where

import qualified Data.Map as Map
import qualified Luna.NodeType as NodeType

data DefManager = DefManager{
	map' :: Map.Map String NodeType.NodeType
} deriving (Show)

empty :: DefManager
empty = DefManager Map.empty

insert :: String -> NodeType.NodeType -> DefManager -> DefManager
insert k v manager =
	manager {map' = Map.insert k v $ map' manager }
