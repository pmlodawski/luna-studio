---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Luna.Interpreter.Session.Profile.Profile where

import qualified Data.Time.Clock                             as Clock
import           Flowbox.Prelude
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Luna.Interpreter.Session.Env.State          as Session
import           Luna.Interpreter.Session.Profile.Info       (ProfileInfo (ProfileInfo))
import qualified Luna.Interpreter.Session.Profile.Info       as ProfileInfo
import           Luna.Interpreter.Session.Session            (Session)
import qualified System.CPUTime                              as CPU



debugNode :: CallPointPath -> Session mm () -> Session mm ()
debugNode callPointPath = Session.reportCompileErrors callPointPath . profile callPointPath

profile :: CallPointPath -> Session mm a -> Session mm a
profile callPointPath action = do
    startCPU  <- liftIO CPU.getCPUTime
    startReal <- liftIO Clock.getCurrentTime
    r <- action
    stopCPU   <- liftIO CPU.getCPUTime
    stopReal  <- liftIO Clock.getCurrentTime
    mergeProfileInfo callPointPath $ ProfileInfo (fromIntegral (stopCPU - startCPU) / (10^12)) (Clock.diffUTCTime stopReal startReal) 0 0
    return r

compileTime :: CallPointPath -> Session mm a -> Session mm a
compileTime = profile' ProfileInfo.compileTime

computeTime :: CallPointPath -> Session mm a -> Session mm a
computeTime = profile' ProfileInfo.computeTime

profile' :: ASetter ProfileInfo ProfileInfo a1 Clock.NominalDiffTime -> CallPointPath  -> Session mm a -> Session mm a
profile' accessor callPointPath action = do
    start <- liftIO Clock.getCurrentTime
    r <- action
    stop <- liftIO Clock.getCurrentTime
    mergeProfileInfo callPointPath $ def & accessor .~ Clock.diffUTCTime stop start
    return r

mergeProfileInfo :: CallPointPath -> ProfileInfo -> Session mm ()
mergeProfileInfo callPointPath = Session.whenVisible callPointPath . Session.mergeProfileInfo callPointPath
