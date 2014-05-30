---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Data.Tuple (
    module X,
    add4to1,
) where

import Data.Tuple as X



add4to1 :: (a, b, c, d) -> e -> (a, b, c, d, e)
add4to1 (a, b, c, d) e = (a, b, c, d, e)
