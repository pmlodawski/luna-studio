---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.List (
    module List,
    module Flowbox.Data.List,
)where

import Data.List as List

import Flowbox.Prelude



foldri :: (a -> b -> b) -> [a] -> b -> b
foldri a b c = foldr a c b

foldli :: (a -> b -> a) -> [b] -> a -> a
foldli a b c = foldl a c b

--foldli :: (a -> b -> b) -> [a] -> b -> b
--foldli a b c = foldr a c b

count :: (a -> Bool) -> [a] -> Int
count predicate = length . List.filter predicate


merge :: (a -> a -> b) -> [a] -> [b]
merge _   []    = []
merge _   [_]   = []
merge fun (h:t) = fun h (head t) : merge fun t
