---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Luna.Interpreter.RPC.Topic where



interpreterGetProjectIDRequest         = "interpreter.getprojectid.request"
interpreterSetProjectIDRequest         = "interpreter.setprojectid.request"
interpreterGetMainPtrRequest           = "interpreter.getmainptr.request"
interpreterSetMainPtrRequest           = "interpreter.setmainptr.request"
interpreterPingRequest                 = "interpreter.ping.request"
interpreterAbortRequest                = "interpreter.abort.request"
interpreterRunRequest                  = "interpreter.run.request"
interpreterSerializationModeDefaultGet = "interpreter.serializationmode.defaultget"
interpreterSerializationModeDefaultSet = "interpreter.serializationmode.defaultset"
interpreterSerializationModeGet        = "interpreter.serializationmode.get"
interpreterSerializationModeSet        = "interpreter.serializationmode.set"
interpreterSerializationModeDelete     = "interpreter.serializationmode.delete"
interpreterWatchPointAddRequest        = "interpreter.watchpoint.add.request"
interpreterWatchPointRemoveRequest     = "interpreter.watchpoint.remove.request"
interpreterWatchPointListRequest       = "interpreter.watchpoint.list.request"
interpreterValueRequest                = "interpreter.value.request"
