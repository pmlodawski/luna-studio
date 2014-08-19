---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.Lib.Edge where

import qualified Luna.Data.Lib.Lib as Lib
import           Flowbox.Prelude



data EdgeCls = Standard deriving (Show, Read, Ord, Eq)


noEdges :: [Edge]
noEdges = []


data Edge = Edge { src :: Lib.ID
                 , dst :: Lib.ID
                 , cls :: EdgeCls
                 } deriving (Show, Read, Ord, Eq)
