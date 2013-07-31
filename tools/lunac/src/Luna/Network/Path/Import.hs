---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Path.Import(
    Import(..),
    noItems,
    simple,
    single,
    multi,
    --genCode
) where

import qualified Luna.Network.Path.Path          as Path
import           Luna.Network.Path.Path            (Path)
import           Data.String.Utils                 (join)

data Import = Import {path :: Path, items :: [String]} deriving (Show)

noItems :: [String]
noItems = []

simple :: Path -> Import
simple path = Import path noItems

single :: Path -> String -> Import
single path item = Import path [item]

multi :: Path -> [String] -> Import
multi path items' = Import path items'
