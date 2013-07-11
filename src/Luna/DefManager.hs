---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
DefManager
--empty
) where

import qualified Luna.Graph as Graph

data DefManager = DefManager{
	rootPackage :: Graph.Node
} deriving (Show)

--empty :: DefManager
--empty = DefManager Graph.Node 

