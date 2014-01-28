---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Monitoring where

import Flowbox.Prelude
-- #ifdef ACCELERATE_ENABLE_EKG
import Control.Monad
--import System.Remote.Monitoring
-- #endif


beginMonitoring :: IO ()
-- #ifdef ACCELERATE_ENABLE_EKG
beginMonitoring = do
  putStrLn "EKG monitor started at: http://localhost:8000\n"
  --void $ forkServer "localhost" 8000
-- #else
--beginMonitoring = return ()
-- #endif

