---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.PluginManager.RPC.Handler.Handler where

import Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message                        (Message)
import           Flowbox.Bus.Data.Prefix                         (Prefix)
import qualified Flowbox.Bus.Data.Prefix                         as Prefix
import           Flowbox.Bus.Data.Topic                          (Topic, status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                      (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                      as HandlerMap
import           Flowbox.Bus.RPC.RPC                             (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor                as Processor
import           Flowbox.PluginManager.Context                   (Context)
import qualified Flowbox.PluginManager.RPC.Handler.Plugin        as PluginHandler
import qualified Flowbox.PluginManager.RPC.Handler.PluginManager as PluginManagerHandler
import qualified Flowbox.PluginManager.RPC.Topic                 as Topic
import           Flowbox.Prelude                                 hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                    as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPC.Handler.Handler"


handlerMap :: Prefix -> HandlerMap Context IO
handlerMap prefix callback = HandlerMap.fromList $ Prefix.prefixifyTopics prefix
    [ (Topic.pluginAddRequest        , respond update PluginHandler.add    )
    , (Topic.pluginRemoveRequest     , respond update PluginHandler.remove )
    , (Topic.pluginListRequest       , respond status PluginHandler.list   )
    , (Topic.pluginLookupRequest     , respond status PluginHandler.lookup )
    , (Topic.pluginStartRequest      , respond update PluginHandler.start  )
    , (Topic.pluginStopRequest       , respond update PluginHandler.stop   )
    , (Topic.pluginRestartRequest    , respond update PluginHandler.restart)
    , (Topic.pluginManagerPingRequest, respond status PluginManagerHandler.ping)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResult
