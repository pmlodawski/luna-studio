---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Control.Monad.State (
    module Flowbox.Control.Monad.State,
    module X
) where

import Flowbox.Prelude
import Control.Monad.State as X hiding (withState)

withState :: MonadState s m => (s -> s) -> m a -> m a
withState f m = do
  s   <- get
  put $ f s
  ret <- m
  put s
  return ret


mapStateVal f = do
  s   <- get
  put $ f s