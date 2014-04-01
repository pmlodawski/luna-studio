---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Data.ByteString (
    module Data.ByteString,
    splitFirsts,
) where

import           Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Flowbox.Prelude



splitFirsts :: Int -> Char -> ByteString -> [ByteString]
splitFirsts count' sep list =
    if count' > 1
        then a : (splitFirsts (count' - 1) sep $ Char8.tail b)
        else [list]
    where (a, b) = Char8.break (== sep) list
