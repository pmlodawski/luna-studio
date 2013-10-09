{-# LANGUAGE OverloadedStrings #-}

module Cabal where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.cabalBin . Cfg.cabalTP . Cfg.thirdparty) cfg
    print exec
    if "install" `elem` args
        then Cmd.rawSystem exec $ ("--config-file=" ++ (Cfg.cabal . Cfg.config) cfg)
                                : "--package-db=clear"
                                : "--package-db=global"
                                : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                                : args
        else Cmd.rawSystem exec $ ("--config-file=" ++ (Cfg.cabal . Cfg.config) cfg)
                                : args

