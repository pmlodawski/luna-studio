---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.HandlerMap (
    module X,
    Callback,
    HandlerMap,
    topics,
    lookupAndCall,
) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Map               as X
import qualified Data.Map               as Map

import           Flowbox.Bus.Data.Message     (Message)
import qualified Flowbox.Bus.Data.Message     as Message
import           Flowbox.Bus.Data.Topic       (Topic)
import           Flowbox.Bus.RPC.RPC          (RPC)
import           Flowbox.Prelude              hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.HandlerMap"


type Callback m = (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC m [result]) -> m [Message]


type HandlerMap m = Callback m -> Map Topic (m [Message])


topics :: HandlerMap m -> [Topic]
topics handlerMap = Map.keys $ handlerMap undefined


lookupAndCall :: MonadIO m => HandlerMap m -> Callback m -> Topic -> m [Message]
lookupAndCall handlerMap callback topic = case Map.lookup topic $ handlerMap callback of
    Just action -> action
    Nothing     -> do let errMsg = "Unknown topic: " ++ show topic
                      logger error errMsg
                      return $ Message.mkError topic errMsg
