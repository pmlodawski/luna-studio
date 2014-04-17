---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.ZMQ.RPC.RPC (
    module Control.Monad.Trans.Either,
    liftIO,
    safeLiftIO,
    RPC,
    Error
) where

import Control.Exception          (SomeException, try)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either
import Data.EitherR               (fmapL)

import Flowbox.Prelude



type RPC a = EitherT Error IO a


type Error = String


safeLiftIO :: IO a -> RPC a
safeLiftIO fun = do
    result <- liftIO $ (try :: IO a -> IO (Either SomeException a)) fun
    hoistEither $ fmapL show result

