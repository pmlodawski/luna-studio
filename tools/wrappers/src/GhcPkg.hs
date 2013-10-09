{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.rawExec . Cfg.ghcPkg) cfg
    Cmd.rawSystem exec $ "--global-package-db"
                       : (Cfg.pkgConf . Cfg.ghc) cfg
                       : "--global"
                       : ("--package-db=" ++ (Cfg.pkgDb . Cfg.usr) cfg)
                       : args
