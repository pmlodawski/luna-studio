{-# LANGUAGE OverloadedStrings #-}

module Ghc where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghcBin . Cfg.ghcTP . Cfg.thirdparty) cfg
    Cmd.rawSystem exec $ ("-B" ++ (Cfg.topDir . Cfg.ghcTP . Cfg.thirdparty) cfg)
                       : "-no-user-package-db" 
                       : ("-package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                       : args