---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.PluginManager.RPC.Handler.Handler where

import Control.Arrow (first)

import           Flowbox.Bus.Data.Message                        (Message)
import           Flowbox.Bus.Data.Topic                          (Topic, status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                      (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                      as HandlerMap
import           Flowbox.Bus.RPC.RPC                             (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor                as Processor
import           Flowbox.PluginManager.Context                   (ContextRef)
import           Flowbox.PluginManager.Prefix                    (Prefix)
import qualified Flowbox.PluginManager.Prefix                    as Prefix
import qualified Flowbox.PluginManager.RPC.Handler.Plugin        as PluginHandler
import qualified Flowbox.PluginManager.RPC.Handler.PluginManager as PluginManagerHandler
import qualified Flowbox.PluginManager.RPC.Topic                 as Topic
import           Flowbox.Prelude                                 hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                    as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPC.Handler.Handler"


prefixifyTopics :: Prefix -> [(Topic, a)] -> [(Topic, a)]
prefixifyTopics prefix = map (first $ Prefix.prefixify prefix)


handlerMap :: Prefix -> ContextRef -> HandlerMap IO
handlerMap prefix ctx callback = HandlerMap.fromList $ prefixifyTopics prefix
    [ (Topic.pluginAddRequest        , respond update $ PluginHandler.add     ctx)
    , (Topic.pluginRemoveRequest     , respond update $ PluginHandler.remove  ctx)
    , (Topic.pluginListRequest       , respond status $ PluginHandler.list    ctx)
    , (Topic.pluginLookupRequest     , respond status $ PluginHandler.lookup  ctx)
    , (Topic.pluginStartRequest      , respond update $ PluginHandler.start   ctx)
    , (Topic.pluginStopRequest       , respond update $ PluginHandler.stop    ctx)
    , (Topic.pluginRestartRequest    , respond update $ PluginHandler.restart ctx)
    , (Topic.pluginManagerPingRequest, respond status PluginManagerHandler.ping)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC IO result) -> IO [Message]
        respond type_ = callback ((/+) type_) . Processor.singleResult
