---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.ProfileInfo where

import qualified Data.Time.Clock as Clock
import qualified System.CPUTime  as CPU

import Flowbox.Prelude



data ProfileInfo = ProfileInfo { _cpuTime  :: Double
                               , _realTime :: Clock.NominalDiffTime
                               } deriving (Show, Eq, Ord)

makeLenses ''ProfileInfo


profile :: MonadIO m => m a -> m (a, ProfileInfo)
profile action = do
    start  <- liftIO CPU.getCPUTime
    start' <- liftIO Clock.getCurrentTime
    r      <- action
    end    <- liftIO CPU.getCPUTime
    end'   <- liftIO Clock.getCurrentTime
    let info = ProfileInfo (fromIntegral     (end - start)  / (10^12))
                           (Clock.diffUTCTime end'  start')
    return (r, info)
