{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import System.Environment
import qualified Filesystem.Path.CurrentOS as Path
import qualified System.Cmd                as Cmd
import qualified Data.Text                 as T
import qualified Config.Config             as Config

pathToString p = T.unpack ptxt where
    Right ptxt = Path.toText p

main = do
    config  <- Config.get
    args    <- getArgs
    let exec = Path.append (Config.ghcTopDir config) "ghc-pkg"
    Cmd.rawSystem (pathToString exec) $ "--global-package-db"
                                      : pathToString (Config.ghcPkgConf config)
                                      : "--global"
                                      : ("--package-db=" ++ pathToString (Config.userPkgDb config))
                                      : args