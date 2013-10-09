{-# LANGUAGE OverloadedStrings #-}

module Ghci where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.exec . Cfg.ghc) cfg
    Cmd.rawSystem exec ("--interactive" : args)
