---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
DefManager(..),
empty,
library
) where

import qualified Luna.Graph   as Graph
import           Luna.Graph     (Graph)
import qualified Luna.Node    as Node
import           Luna.Node      (Node)
import qualified Luna.NodeDef as NodeDef
import qualified Data.Map     as Map
import           Data.Map       (Map)
import qualified Luna.Library as Library
import           Luna.Library   (Library)


data DefManager = DefManager{
	libraries :: Map Library.LibID Library,
	defs      :: Graph
} deriving (Show)

empty :: DefManager
empty = DefManager Map.empty Graph.empty

library :: Node -> DefManager -> Maybe Library
library node manager = Map.lookup (NodeDef.libID $ Node.def node) $ libraries manager

--load :: 

