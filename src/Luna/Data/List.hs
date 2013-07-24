---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Data.List(
    foldri
) where

foldri :: (a -> b -> b) -> [a] -> b -> b
foldri a b c = foldr a c b

--foldli :: (a -> b -> b) -> [a] -> b -> b
--foldli a b c = foldr a c b