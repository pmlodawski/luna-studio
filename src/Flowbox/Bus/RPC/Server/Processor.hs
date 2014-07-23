---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Server.Processor where

import Control.Monad             (liftM)
import Control.Monad.IO.Class    (MonadIO)
import Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message     (Message)
import qualified Flowbox.Bus.Data.Message     as Message
import           Flowbox.Bus.Data.Topic       (Topic)
import           Flowbox.Bus.RPC.HandlerMap   (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap   as HandlerMap
import qualified Flowbox.Bus.RPC.RPC          as RPC
import           Flowbox.Prelude              hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers as Proto
import           Generated.Proto.Rpc.Response (Response)


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Server.Processor"


singleResult :: MonadIO m => (a -> m b) -> a -> m [b]
singleResult f a = liftM mkList $ f a


noResult :: MonadIO m => (a -> m ()) -> a -> m [Response]
noResult f a = f a >> return []


-- FIXME [PM] : Code duplication

process :: HandlerMap s IO -> Message -> StateT s IO [Message]
process handlerMap msg = HandlerMap.lookupAndCall handlerMap call topic where
    call mkTopic method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.run $ method args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right ok -> map (respond mkTopic) ok

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => (Topic -> Topic) -> msg -> Message
    respond mkTopic = Message.mk (mkTopic topic)


processLifted :: MonadIO m => HandlerMap s m -> Message -> StateT s m [Message]
processLifted handlerMap msg = HandlerMap.lookupAndCall handlerMap call topic where
    call mkTopic method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.runLifted $ method args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right ok -> map (respond mkTopic) ok

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => (Topic -> Topic) -> msg -> Message
    respond mkTopic = Message.mk (mkTopic topic)

