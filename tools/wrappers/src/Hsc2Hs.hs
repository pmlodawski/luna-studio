{-# LANGUAGE OverloadedStrings #-}

module Hsc2Hs where

import qualified System.Environment    as Env
import qualified System.Cmd            as Cmd
import qualified Flowbox.Config.Config as Cfg


main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let topDir = (Cfg.topDir . Cfg.ghcTP . Cfg.thirdparty) cfg
        tflag  = "--template=" ++ topDir ++ "/template-hsc.h"
        iflag  = "-I" ++ topDir ++ "/include/"
        exec   = (Cfg.hsc2hsBin . Cfg.ghcTP . Cfg.thirdparty) cfg

    -- FIXME: handle Darwin

    Cmd.rawSystem exec $ (tflag
                       : "--cflag=-fno-stack-protector"
                       : args
                       ) ++ [iflag]



