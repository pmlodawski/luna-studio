---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Data.Time (
    module Data.Time,
    toSeconds,

) where

import Data.Time
import Flowbox.Prelude



toSeconds :: NominalDiffTime -> Int
toSeconds nominalDiffTime = (fromEnum nominalDiffTime) `div` e12


e12 :: Int
e12 = 1000000000000
