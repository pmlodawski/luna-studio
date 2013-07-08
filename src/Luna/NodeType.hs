---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.NodeType(
NodeType(..)
) where

import qualified Luna.NodeDef as NodeDef

data NodeType = Function NodeDef.NodeDef 
	| Package NodeDef.NodeDef 
	| Class NodeDef.NodeDef 
	| Interface NodeDef.NodeDef
	deriving (Show)
