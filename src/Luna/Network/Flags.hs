---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Flags(
    Flags(..),
    empty
) where

data Flags = Flags {
                 io :: Bool,
                 omit :: Bool
             } deriving (Show)

empty :: Flags
empty = Flags False False
