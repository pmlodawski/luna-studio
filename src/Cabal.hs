{-# LANGUAGE OverloadedStrings #-}

module Cabal where

import qualified System.Cmd            as Cmd
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg



main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.cabalBin . Cfg.cabalTP . Cfg.thirdparty) cfg
    let flags = if "--global" `elem` args
                then ["--package-db=clear", "--package-db=global", "--package-db=" ++ (Cfg.pkgDb . Cfg.local)  cfg, "--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg]
                else ["--package-db=clear", "--package-db=global", "--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg, "--package-db=" ++ (Cfg.pkgDb . Cfg.local)  cfg]
    exitCode <- if "install" `elem` args
                    then Cmd.rawSystem exec $ ["--config-file=" ++ (Cfg.cabal . Cfg.config) cfg]
                                            ++ flags
                                            ++ args
                                            -- : "--package-db=clear"
                                            -- : "--package-db=global"
                                            -- : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                                            -- : args
                    else Cmd.rawSystem exec $ ("--config-file=" ++ (Cfg.cabal . Cfg.config) cfg)
                                            : args
    Exit.exitWith exitCode
