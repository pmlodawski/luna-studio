---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Shuffle where

import Control.PolyMonad (PolyMonad, polyJoin)
import Control.Monad (join)
import Prelude

class Shuffle m1 m2 where
    shuffle :: m1 (m2 a) -> m2 (m1 a)

shuffleJoin :: (Shuffle n1 m2, PolyMonad m1 m2 mout, PolyMonad n1 n2 nout, Functor m1, Functor mout)
            => m1 (n1 (m2 (n2 a))) -> mout (nout a)
shuffleJoin = fmap polyJoin . polyJoin . fmap shuffle

infixl  1 >>>=
infixl  1 $>>=
	
(>>>=) = deepBind 

($>>=) :: (Monad m, Monad t, Functor t, Shuffle m t) => m a -> (a -> t (m b)) -> t (m b)
a $>>= b = return a >>>= b

deepBind :: (Monad m, Monad t, Functor t, Shuffle m t) => t (m a) -> (a -> t (m b)) -> t (m b)
deepBind tma f = tma >>= mf
    where mf ma = fmap join . shuffle $ do
              a <- ma
              return $ f a

instance (Monad m, Functor m) => Shuffle (Either e) m where
	shuffle = \case
		Left  e -> return $ Left e
		Right a -> fmap Right a
