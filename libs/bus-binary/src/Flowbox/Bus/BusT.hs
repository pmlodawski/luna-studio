---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Bus.BusT where

import           Control.Monad.IO.Class

import           Flowbox.Bus.Bus        (Bus)
import           Flowbox.Prelude


--FIXME[PM] : rename to BusWrapper
-- and implement as: newtype BusT a = BusT { runBusT :: Bus a} deriving (Monad, MonadIO)
-- and in other file
newtype BusT a = BusT { runBusT :: Bus a}

instance Functor BusT where
    fmap f (BusT a) = BusT $ f <$> a

instance Applicative BusT where
    pure a = BusT $ pure a
    (BusT f) <*> (BusT a) = BusT $ f <*> a

instance Monad BusT where
    (BusT a) >>= f = BusT $ a >>= runBusT . f


instance MonadIO BusT where
    liftIO a = BusT $ liftIO a

