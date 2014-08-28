---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.SessionT where

import Control.Monad.IO.Class

import Flowbox.Prelude
import Luna.Interpreter.Session.Session (Session)



newtype SessionT a = SessionT { runSessionT :: Session a}


instance Monad SessionT where
    return a = SessionT $ return a
    (SessionT a) >>= f = SessionT $ a >>= runSessionT . f


instance MonadIO SessionT where
    liftIO a = SessionT $ liftIO a


instance Functor SessionT where
    fmap f = SessionT . fmap f . runSessionT
