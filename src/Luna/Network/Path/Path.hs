---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Path.Path(
    Path(..),
    append,
    prepend
) where

newtype Path = Path {segments :: [String]} deriving (Show)

append :: String -> Path -> Path
append segment path = Path $ (segments path) ++ [segment]

prepend :: String -> Path -> Path
prepend segment path = Path $ segment:(segments path)