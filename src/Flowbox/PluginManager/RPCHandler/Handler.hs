---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.PluginManager.RPCHandler.Handler where

import           Flowbox.Bus.Data.Message                (Message)
import qualified Flowbox.Bus.Data.Topic                  as Topic
import           Flowbox.Bus.RPC.HandlerMap              (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap              as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor        as Processor
import           Flowbox.PluginManager.Context           (ContextRef)
import qualified Flowbox.PluginManager.RPCHandler.Plugin as PluginHandler
import           Flowbox.Prelude                         hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers            as Proto


logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPCHandler.Handler"


handlerMap :: ContextRef -> HandlerMap
handlerMap ctx callback = HandlerMap.fromList $
    [ ("plugin.add.request"    , call Topic.update $ PluginHandler.add     ctx)
    , ("plugin.remove.request" , call Topic.update $ PluginHandler.remove  ctx)
    , ("plugin.list.request"   , call Topic.status $ PluginHandler.list    ctx)
    , ("plugin.lookup.request" , call Topic.status $ PluginHandler.lookup  ctx)
    , ("plugin.start.request"  , call Topic.update $ PluginHandler.start   ctx)
    , ("plugin.stop.request"   , call Topic.update $ PluginHandler.stop    ctx)
    , ("plugin.restart.request", call Topic.update $ PluginHandler.restart ctx)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> IO result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult
