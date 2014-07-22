---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Interpreter.RPC.Topic where



interpreterWatchPointAddRequest    = "interpreter.watchpoint.add.request"
interpreterWatchPointRemoveRequest = "interpreter.watchpoint.remove.request"
interpreterWatchPointListRequest   = "interpreter.watchpoint.list.request"
interpreterInvalidateCallRequest   = "interpreter.invalidate.call.request"
interpreterInvalidateDefRequest    = "interpreter.invalidate.def.request"
interpreterInvalidateNodeRequest   = "interpreter.invalidate.node.request"
interpreterRunRequest              = "interpreter.run.request"
