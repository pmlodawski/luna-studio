{-# LANGUAGE OverloadedStrings #-}

module Ghc where

import System.Environment
import qualified Filesystem.Path.CurrentOS as Path
import qualified System.Cmd                as Cmd
import qualified Data.Text                 as T
import qualified Flowbox.Config.Config     as Cfg

pathToString p = T.unpack ptxt where
    Right ptxt = Path.toText p

main = do
	cfg     <- Cfg.load
	print $ (Cfg.path . Cfg.ghc) cfg
    --config  <- Config.get
    --args    <- getArgs
    --let exec = Path.append (Config.ghcTopDir config) "ghc"
    --Cmd.rawSystem (pathToString exec) $ ("-B" ++ pathToString (Config.ghcTopDir config))
    --                                  : "-no-user-package-db" 
    --                                  : ("-package-db=" ++ pathToString (Config.userPkgDb config))
    --                                  : args