---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Luna.Target.HS.Control.Context.Monad where

class Monad' m where
  (#>>=) :: m a -> (a -> m b) -> m b
  (#>>) :: m a -> m b -> m b
  return' :: a -> m a


instance Monad' m where
    (#>>=)  = error ("Std.UndefinedMonad")
    (#>>)   = error ("Std.UndefinedMonad")
    return' = error ("Std.UndefinedMonad")