---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Node(
Node(..)
) where

import qualified Luna.DefaultValue as DefaultValue

data Node = Node { defName :: String }
		  | DefaultNode { defValue :: DefaultValue.DefaultValue }
		  deriving (Show, Read)
