{-# LANGUAGE OverloadedStrings #-}

module Ghc where

import qualified System.Cmd            as Cmd
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg



main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghcBin . Cfg.ghcTP . Cfg.thirdparty) cfg
    exitCode <- Cmd.rawSystem exec $ ("-B" ++ (Cfg.topDir . Cfg.ghcTP . Cfg.thirdparty) cfg)
                       : "-no-user-package-db" 
                       : ("-package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                       : args
    Exit.exitWith exitCode
