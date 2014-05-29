---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Control.Concurrent (
    module Control.Concurrent,
    forkIO_,
    forkIO',
    waitThread,
) where

import Control.Concurrent

import Flowbox.Prelude



forkIO_ :: IO () -> IO ()
forkIO_ a = void $ forkIO a


forkIO' :: IO a -> IO (MVar ())
forkIO' io = do
    mvar <- newEmptyMVar
    _ <- forkFinally io (\_ -> putMVar mvar ())
    return mvar


waitThread :: MVar a -> IO a
waitThread = takeMVar
