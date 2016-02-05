---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Cabal where

import qualified System.Environment      as Env
import qualified System.Exit             as Exit
import qualified System.Process          as Process

import qualified Flowbox.Config.Config   as Cfg
import           Flowbox.System.FilePath (expand')



main :: IO ()
main = do
    cfg        <- Cfg.load
    args       <- Env.getArgs
    exec       <- expand' $ Cfg.cabal $ Cfg.bins   cfg
    localPkgDb <- expand' $ Cfg.pkgDb $ Cfg.local  cfg
    globalPkgD <- expand' $ Cfg.pkgDb $ Cfg.global cfg
    cabalCfg   <- expand' $ Cfg.cabal $ Cfg.config cfg
    let flags = if "--global" `elem` args
                then [ "--package-db=clear"
                     , "--package-db=global"
                     , "--package-db=" ++ localPkgDb
                     , "--package-db=" ++ globalPkgD
                     ]
                else [ "--package-db=clear"
                     , "--package-db=global"
                     , "--package-db=" ++ globalPkgD
                     , "--package-db=" ++ localPkgDb
                     ]

        allArgs = ("--config-file=" ++ cabalCfg)
                : (if "install" `elem` args || "configure" `elem` args
                   then flags else [])
               ++ args
    Process.rawSystem exec allArgs >>= Exit.exitWith
