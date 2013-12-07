{-# LANGUAGE OverloadedStrings #-}

module Ghci where

import qualified System.Cmd         as Cmd
import qualified System.Environment as Env
import qualified System.Exit        as Exit

import qualified Flowbox.Config.Config as Cfg



main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghc . Cfg.wrappers) cfg
    exitCode <- Cmd.rawSystem exec ("--interactive" : args)
    Exit.exitWith exitCode
