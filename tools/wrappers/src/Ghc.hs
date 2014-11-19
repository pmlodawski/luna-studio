---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Ghc where

import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified System.Process     as Process

import qualified Flowbox.Config.Config   as Cfg
import           Flowbox.System.FilePath (expand')



main :: IO ()
main = do
    cfg        <- Cfg.load
    args       <- Env.getArgs
    exec       <- expand' $ Cfg.ghc    $ Cfg.bins cfg
    topDir     <- expand' $ Cfg.topDir $ Cfg.ghcS cfg
    localPkgDb <- expand' $ Cfg.pkgDb  $ Cfg.local  cfg
    globalPkgD <- expand' $ Cfg.pkgDb  $ Cfg.global cfg
    let allArgs = ("-B" ++ topDir)
                : "-no-user-package-db"
                : ("-package-db=" ++ globalPkgD)
                : ("-package-db=" ++ localPkgDb)
                : args
    Process.rawSystem exec allArgs >>= Exit.exitWith
