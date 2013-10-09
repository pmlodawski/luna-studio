{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import qualified System.Environment        as Env
import qualified System.Cmd                as Cmd
import qualified Flowbox.Config.Config     as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghcPkgBin . Cfg.ghcTP . Cfg.thirdparty) cfg
    Cmd.rawSystem exec $ "--global-package-db"
                       : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
                       : "--global"
                       : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                       : args
