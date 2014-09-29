---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Edge where

import Flowbox.Prelude



noEdges :: [Edge]
noEdges = []


data Edge = Standard deriving (Show, Read, Ord, Eq)
