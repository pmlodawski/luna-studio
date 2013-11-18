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
    appendFile "C:\\test.txt" "ghc\n"
    appendFile "C:\\test.txt" (show args)
    appendFile "C:\\test.txt" "\n---\n"

    exitCode <- Cmd.rawSystem exec $ --("-B" ++ (Cfg.topDir . Cfg.ghcTP . Cfg.thirdparty) cfg)
                       "-no-user-package-db" 
                       : ("-package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg)
                       : ("-package-db=" ++ (Cfg.pkgDb . Cfg.local)  cfg)
                       : args
    Exit.exitWith exitCode
