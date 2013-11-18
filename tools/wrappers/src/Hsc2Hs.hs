{-# LANGUAGE OverloadedStrings #-}

module Hsc2Hs where

import qualified System.Cmd            as Cmd
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg


main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let topDir = (Cfg.topDir . Cfg.ghcTP . Cfg.thirdparty) cfg
        tflag  = "--template=" ++ topDir ++ "/template-hsc.h"
        iflag  = "-I" ++ topDir ++ "/include/"
        exec   = (Cfg.hsc2hsBin . Cfg.ghcTP . Cfg.thirdparty) cfg

    -- FIXME: handle Darwin

    appendFile "C:\\test.txt" "hsc2hs\n"
    appendFile "C:\\test.txt" (show args)
    appendFile "C:\\test.txt" "\n---\n"

    exitCode <- Cmd.rawSystem exec $ (tflag
                       : "--cflag=-fno-stack-protector"
                       : args
                       ) ++ [iflag]
    Exit.exitWith exitCode
