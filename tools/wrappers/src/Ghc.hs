{-# LANGUAGE OverloadedStrings #-}

module Ghc where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.rawExec . Cfg.ghc) cfg
    Cmd.rawSystem exec $ ("-B" ++ (Cfg.topDir . Cfg.ghc) cfg)
                       : "-no-user-package-db" 
                       : ("-package-db=" ++ (Cfg.pkgDb . Cfg.usr) cfg)
                       : args