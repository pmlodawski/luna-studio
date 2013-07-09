---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.NodeType(
NodeType(..)
) where

import Luna.NodeDef (NodeDef)
import qualified Luna.Graph as Graph

data NodeType = Function NodeDef 
	| Package NodeDef 
	| Class NodeDef 
	| Interface NodeDef
	deriving (Show)

--class NodeTypeC a where
--	test :: a -> Bool

--instance NodeTypeC NodeType where
--	test _ = False

-- empty = a $ \name = NodeDef name Graph.empty [] []

