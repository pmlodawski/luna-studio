-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Control.PolyMonad where

import Prelude

class PolyMonad m1 m2 m3 | m1 m2 -> m3 where
    (>>>=) :: m1 a -> (a -> m2 b) -> m3 b

polyBind :: PolyMonad m1 m2 m3 => m1 a -> (a -> m2 b) -> m3 b
polyBind = (>>>=)

polyJoin :: PolyMonad m1 m2 m3 => m1 (m2 a) -> m3 a
polyJoin = (>>>= id)

