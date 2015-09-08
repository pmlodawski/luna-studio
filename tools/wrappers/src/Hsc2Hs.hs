---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Hsc2Hs where

import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified System.Process     as Process

import qualified Flowbox.Config.Config   as Cfg
import           Flowbox.System.FilePath (expand')



main :: IO ()
main = do
    cfg    <- Cfg.load
    args   <- Env.getArgs
    topDir <- expand' $ Cfg.topDir $ Cfg.ghcS cfg
    exec   <- expand' $ Cfg.hsc2hs $ Cfg.bins cfg
    let tflag  = "--template=" ++ topDir ++ "/template-hsc.h"
        iflag  = "-I" ++ topDir ++ "/include/"
        -- FIXME: handle Darwin
        allArgs = tflag
                : "--cflag=-fno-stack-protector"
                : args
               ++ [iflag]

    Process.rawSystem exec allArgs >>= Exit.exitWith
