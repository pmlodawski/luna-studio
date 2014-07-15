---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Interpreter.RPC.Topic where



interpreterWatchPointAdd    = "interpreter.watchpoint.add"
interpreterWatchPointRemove = "interpreter.watchpoint.remove"
interpreterWatchPointList   = "interpreter.watchpoint.list"
interpreterInvalidateCall   = "interpreter.invalidate.call"
interpreterInvalidateDef    = "interpreter.invalidate.def"
interpreterInvalidateNode   = "interpreter.invalidate.node"
interpreterRun              = "interpreter.run"
