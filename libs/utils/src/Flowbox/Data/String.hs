---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.String where

import qualified Data.Char as Char

import Flowbox.Prelude


toUpper :: String -> String
toUpper n = Char.toUpper (head n) : tail n
