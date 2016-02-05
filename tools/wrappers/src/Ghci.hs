---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Ghci where

import qualified System.Environment      as Env
import qualified System.Exit             as Exit
import qualified System.Process          as Process

import qualified Flowbox.Config.Config   as Cfg
import           Flowbox.System.FilePath (expand')



main :: IO ()
main = do
    cfg  <- Cfg.load
    args <- Env.getArgs
    exec <- expand' $ Cfg.ghc $ Cfg.wrappers cfg
    Process.rawSystem exec ("--interactive" : args) >>= Exit.exitWith
