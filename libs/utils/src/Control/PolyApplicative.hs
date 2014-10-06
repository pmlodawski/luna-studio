---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Control.PolyApplicative where

class PolyApplicative m1 m2 m3 | m1 m2 -> m3 where
    (<<*>>) :: m1 (a -> b) -> m2 a -> m3 b



