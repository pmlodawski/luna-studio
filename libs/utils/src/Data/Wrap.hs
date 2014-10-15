---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}

module Data.Wrap where

import Control.Monad.Trans
import Prelude

class Wrap m where
    wrap   :: a   -> m a
    unwrap :: m a -> a

    default wrap :: Monad m => a -> m a
    wrap = return

class WrapT t where
    wrapT   :: m a   -> t m a
    unwrapT :: t m a -> m a

    default wrapT :: (MonadTrans t, Monad m) => m a -> t m a
    wrapT = lift




