---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Graph.Port where

import Flowbox.Prelude



data Port = All
          | Num Int
          deriving (Eq, Show, Ord, Read)


toList :: Port -> [Int]
toList All     = []
toList (Num a) = [a]
