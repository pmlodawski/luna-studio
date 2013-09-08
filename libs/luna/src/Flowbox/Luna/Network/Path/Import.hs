---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Path.Import(
    Import(..),
    noItems,
    simple,
    single,
    multi,
    --genCode
) where

import           Flowbox.Prelude                  
import           Flowbox.Luna.Network.Path.Path   (Path)

data Import = Import {path :: Path, items :: [String]} deriving (Show)

noItems :: [String]
noItems = []

simple :: Path -> Import
simple p = Import p noItems

single :: Path -> String -> Import
single p item = Import p [item]

multi :: Path -> [String] -> Import
multi p items' = Import p items'
