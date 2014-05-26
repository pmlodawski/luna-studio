---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}

module Flowbox.PluginManager.RPCHandler.Handler where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Flowbox.Bus.Data.Message                (Message)
import           Flowbox.Bus.Data.Topic                  (Topic)
import           Flowbox.Bus.RPC.Handler                 (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Server.Processor        as P
import           Flowbox.PluginManager.Context           (ContextRef)
import qualified Flowbox.PluginManager.RPCHandler.Plugin as PluginHandler
import           Flowbox.Prelude                         hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers            as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPCHandler.Handler"


topics :: [Topic]
topics = Map.keys $ methods undefined undefined


handler :: ContextRef -> BusRPCHandler
handler ctx callback topic = case Map.lookup topic $ methods ctx callback of
    Just action -> action
    Nothing     -> do let errMsg = "Unknown topic: " ++ show topic
                      logger error errMsg
                      return $ P.respondError topic errMsg


methods :: ContextRef -> P.Callback -> Map Topic (IO [Message])
methods ctx callback = Map.fromList $
    [ ("plugin.add.request"    , call $ PluginHandler.add     ctx)
    , ("plugin.remove.request" , call $ PluginHandler.remove  ctx)
    , ("plugin.list.request"   , call $ PluginHandler.list    ctx)
    , ("plugin.lookup.request" , call $ PluginHandler.lookup  ctx)
    , ("plugin.start.request"  , call $ PluginHandler.start   ctx)
    , ("plugin.stop.request"   , call $ PluginHandler.stop    ctx)
    , ("plugin.restart.request", call $ PluginHandler.restart ctx)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => (args -> IO result) -> IO [Message]
        call = callback P.update . P.singleResult
