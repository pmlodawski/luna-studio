---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.PluginManager.Handler.Handler where

import           Flowbox.Bus.RPC.BusRPCHandler        (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor            as P
import           Flowbox.Bus.Topic                    (Topic)
import           Flowbox.PluginManager.Context        (ContextRef)
import qualified Flowbox.PluginManager.Handler.Plugin as PluginHandler
import           Flowbox.Prelude                      hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.Processor"


topics :: [Topic]
topics = [ "plugin.add.request"
         , "plugin.remove.request"
         , "plugin.list.request"
         , "plugin.lookup.request"
         , "plugin.start.request"
         , "plugin.stop.request"
         ]


handler :: ContextRef -> BusRPCHandler
handler ctx callback topic = case topic of
    "plugin.add.request"    -> callback P.update $ PluginHandler.add    ctx
    "plugin.remove.request" -> callback P.update $ PluginHandler.remove ctx
    "plugin.list.request"   -> callback P.status $ PluginHandler.list   ctx
    "plugin.lookup.request" -> callback P.status $ PluginHandler.lookup ctx
    "plugin.start.request"  -> callback P.update $ PluginHandler.start  ctx
    "plugin.stop.request"   -> callback P.update $ PluginHandler.stop   ctx
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg
