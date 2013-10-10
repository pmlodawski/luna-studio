{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import qualified System.Cmd            as Cmd
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg



main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghcPkgBin . Cfg.ghcTP . Cfg.thirdparty) cfg
    exitCode <- Cmd.rawSystem exec $ "--global-package-db"
                       : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
                       : "--global"
                       : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                       : args
    Exit.exitWith exitCode
    
