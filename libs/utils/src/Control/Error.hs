---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Control.Error (
    module Control.Error,
    module X,
) where

import           Control.Monad.Trans.Except as X
import qualified Data.Maybe                 as Maybe
import           Flowbox.Prelude



infixl 4 <??>
(<??>) :: Monad m => Maybe b -> a -> ExceptT a m b
val <??> m = Maybe.maybe (throwE m) return val


infixl 4 <??&>
(<??&>) :: Monad m => ExceptT a m (Maybe b) -> a -> ExceptT a m b
val <??&> m = Maybe.maybe (throwE m) return =<< val
