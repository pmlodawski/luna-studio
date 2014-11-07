---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Luna.Interpreter.RPC.Topic where



interpreterGetProjectIDRequest                = "interpreter.getprojectid.request"
interpreterSetProjectIDRequest                = "interpreter.setprojectid.request"
interpreterGetMainPtrRequest                  = "interpreter.getmainptr.request"
interpreterSetMainPtrRequest                  = "interpreter.setmainptr.request"
interpreterPingRequest                        = "interpreter.ping.request"
interpreterAbortRequest                       = "interpreter.abort.request"
interpreterRunRequest                         = "interpreter.run.request"
interpreterSerializationModeDefaultGetRequest = "interpreter.serializationmode.defaultget.request"
interpreterSerializationModeDefaultSetRequest = "interpreter.serializationmode.defaultset.request"
interpreterSerializationModeGetRequest        = "interpreter.serializationmode.get.request"
interpreterSerializationModeSetRequest        = "interpreter.serializationmode.set.request"
interpreterSerializationModeDeleteRequest     = "interpreter.serializationmode.delete.request"
interpreterWatchPointAddRequest               = "interpreter.watchpoint.add.request"
interpreterWatchPointRemoveRequest            = "interpreter.watchpoint.remove.request"
interpreterWatchPointListRequest              = "interpreter.watchpoint.list.request"
interpreterValueRequest                       = "interpreter.value.request"
interpreterMemoryGetLimitsRequest             = "interpreter.memory.getlimits.request"
interpreterMemorySetLimitsRequest             = "interpreter.memory.setlimits.request"
