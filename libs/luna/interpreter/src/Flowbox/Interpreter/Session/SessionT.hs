---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.SessionT where

import Control.Monad.State

import Flowbox.Interpreter.Session.Session (Session)
import Flowbox.Prelude





newtype SessionT a = SessionT { runSessionT :: Session a}


instance Monad SessionT where
    return a = SessionT $ return a
    (SessionT a) >>= f = SessionT $ a >>= runSessionT . f
