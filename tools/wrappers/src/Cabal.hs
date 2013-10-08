{-# LANGUAGE OverloadedStrings #-}

module Cabal where

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
    let exec = Path.append (Config.cabalBinDir config) "cabal"
    if "install" `elem` args
        then Cmd.rawSystem (pathToString exec) $ ("--config-file=" ++ pathToString (Config.cabalConf config))
                                               : "--package-db=clear"
                                               : "--package-db=global"
                                               : ("--package-db=" ++ pathToString (Config.userPkgDb config))
                                               : args
        else Cmd.rawSystem (pathToString exec) $ ("--config-file=" ++ pathToString (Config.cabalConf config))
                                               : args