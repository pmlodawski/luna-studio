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
interpreterInvalidateRequest                  = "interpreter.invalidate.request"
interpreterVarTimeGetRequest                  = "interpreter.var.time.get.request"
interpreterVarTimeSetRequest                  = "interpreter.var.time.set.request"
interpreterSerializationModeGetRequest        = "interpreter.serializationmode.get.request"
interpreterSerializationModeInsertRequest     = "interpreter.serializationmode.insert.request"
interpreterSerializationModeDeleteRequest     = "interpreter.serializationmode.delete.request"
interpreterSerializationModeDeleteAllRequest  = "interpreter.serializationmode.deleteall.request"
interpreterWatchPointAddRequest               = "interpreter.watchpoint.add.request"
interpreterWatchPointRemoveRequest            = "interpreter.watchpoint.remove.request"
interpreterWatchPointListRequest              = "interpreter.watchpoint.list.request"
interpreterValueRequest                       = "interpreter.value.request"
interpreterMemoryGetLimitsRequest             = "interpreter.memory.getlimits.request"
interpreterMemorySetLimitsRequest             = "interpreter.memory.setlimits.request"
interpreterExitRequest                        = "interpreter.exit.request"

rendererRenderRequest     = "renderer.render.request"
rendererRenderNodeRequest = "renderer.rendernode.request"
