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
import           Flowbox.Bus.Data.Topic                          (Topic)
import qualified Flowbox.Bus.Data.Topic                          as Topic
import           Flowbox.Bus.RPC.HandlerMap                      (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                      as HandlerMap
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


handlerMap :: Prefix -> ContextRef -> HandlerMap
handlerMap prefix ctx callback = HandlerMap.fromList $ prefixifyTopics prefix $
    [ (Topic.pluginAddRequest        , call Topic.update $ PluginHandler.add     ctx)
    , (Topic.pluginRemoveRequest     , call Topic.update $ PluginHandler.remove  ctx)
    , (Topic.pluginListRequest       , call Topic.status $ PluginHandler.list    ctx)
    , (Topic.pluginLookupRequest     , call Topic.status $ PluginHandler.lookup  ctx)
    , (Topic.pluginStartRequest      , call Topic.update $ PluginHandler.start   ctx)
    , (Topic.pluginStopRequest       , call Topic.update $ PluginHandler.stop    ctx)
    , (Topic.pluginRestartRequest    , call Topic.update $ PluginHandler.restart ctx)
    , (Topic.pluginManagerPingRequest, call Topic.status $ PluginManagerHandler.ping)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> IO result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult
