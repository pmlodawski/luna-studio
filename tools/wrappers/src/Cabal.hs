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
    let exec = (Cfg.cabal . Cfg.bins) cfg
    let flags = if "--global" `elem` args
                then ["--package-db=clear", "--package-db=global", "--package-db=" ++ (Cfg.pkgDb . Cfg.local)  cfg, "--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg]
                else ["--package-db=clear", "--package-db=global", "--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg, "--package-db=" ++ (Cfg.pkgDb . Cfg.local)  cfg]

    let xargs = args
    --appendFile "C:\\test.txt" "cabal\n"
    --appendFile "C:\\test.txt" (show args)
    --appendFile "C:\\test.txt" "\n---\n"
    --print "!!!"
    --print (show flags)
    exitCode <- if "install" `elem` xargs
                    then Cmd.rawSystem exec $ ["--config-file=" ++ (Cfg.cabal . Cfg.config) cfg]
                                            ++ flags
                                            ++ xargs
                    else Cmd.rawSystem exec $ ("--config-file=" ++ (Cfg.cabal . Cfg.config) cfg)
                                            : xargs
    Exit.exitWith exitCode
