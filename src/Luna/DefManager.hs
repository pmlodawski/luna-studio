---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
empty,
--insert
) where

import qualified Data.Map as Map
import Luna.NodeType (NodeType)
import qualified Luna.DefManager.DefTree as DefTree

data DefManager = DefManager{
	defTree :: DefTree.DefTree
} deriving (Show)

empty :: DefManager
empty = DefManager DefTree.empty

--insert :: TypePath -> NodeType -> DefManager -> DefManager
--insert typePath nodeType defManager = DefTree.insert typePath nodeType $defTree defManager


--insert :: String -> NodeType.NodeType -> DefManager -> DefManager
--insert k v manager =
--	manager {map' = Map.insert k v $ map' manager }
