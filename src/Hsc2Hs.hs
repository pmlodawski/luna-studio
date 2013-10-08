{-# LANGUAGE OverloadedStrings #-}

module Hsc2Hs where

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
    let topDir = (Config.ghcTopDir config)
        exec   = Path.append topDir "hsc2hs"
        tflag  = "--template=" ++ pathToString topDir ++ "/template-hsc.h"
        iflag  = "-I" ++ pathToString topDir ++ "/include/"

    -- FIXME: handle Darwin

    Cmd.rawSystem (pathToString exec) $ (tflag
                                        : "--cflag=-fno-stack-protector"
                                        : args
                                        ) ++ [iflag]

