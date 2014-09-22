---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Shuffle where

import Control.PolyMonad
import Prelude

class Shuffle m1 m2 where
    shuffle :: m1 (m2 a) -> m2 (m1 a)

shuffleJoin :: (Shuffle n1 m2, PolyMonad m1 m2 mout, PolyMonad n1 n2 nout, Functor m1, Functor mout)
            => m1 (n1 (m2 (n2 a))) -> mout (nout a)
shuffleJoin = fmap polyJoin . polyJoin . fmap shuffle
