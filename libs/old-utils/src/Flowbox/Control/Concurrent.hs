---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Control.Concurrent (
    module Control.Concurrent,
    module Flowbox.Control.Concurrent,
) where

import           Control.Concurrent
import qualified Control.Exception  as Exception

import Flowbox.Prelude



forkIO_ :: IO () -> IO ()
forkIO_ a = void $ forkIO a


forkIOwithExceptions_ :: IO () -> IO ()
forkIOwithExceptions_ action = do
    mainThreadId <- myThreadId
    forkIO_ $ Exception.handle (throwTo mainThreadId :: Exception.SomeException -> IO ()) action
