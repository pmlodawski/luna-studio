---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.Debug where

import qualified GHC

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.GHC.Util (dshow)
import Luna.Interpreter.Session.Session  (Session)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


dumpBindings :: Session ()
dumpBindings = do
    bindings <- lift2 GHC.getBindings
    dflags   <- lift2 GHC.getSessionDynFlags
    logger trace "== declared variables =="
    mapM_ (logger trace . dshow dflags) bindings
    logger trace "========================"

