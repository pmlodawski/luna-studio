---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Edge where

import           Flowbox.Prelude
import qualified Luna.Lib.Lib    as Lib



data EdgeCls = Standard deriving (Show, Read, Ord, Eq)


noEdges :: [Edge]
noEdges = []


data Edge = Edge { src :: Lib.ID
                 , dst :: Lib.ID
                 , cls :: EdgeCls
                 } deriving (Show, Read, Ord, Eq)
