---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Bus.BusT where

import Control.Monad.IO.Class

import Flowbox.Bus.Bus (Bus)
import Flowbox.Prelude



newtype BusT a = BusT { runBusT :: Bus a}


instance Monad BusT where
    return a = BusT $ return a
    (BusT a) >>= f = BusT $ a >>= runBusT . f


instance MonadIO BusT where
    liftIO a = BusT $ liftIO a

