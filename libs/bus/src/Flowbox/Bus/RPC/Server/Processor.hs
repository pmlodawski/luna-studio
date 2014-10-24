---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.RPC.Server.Processor where

import           Control.Monad             (liftM)
import qualified Control.Monad.Catch       as Catch
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.State
import qualified Data.Maybe                as Maybe

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
logger = getLoggerIO $(moduleName)


singleResult :: MonadIO m => (a -> m b) -> a -> m [b]
singleResult f a = liftM mkList $ f a


noResult :: MonadIO m => (a -> m ()) -> a -> m [Response]
noResult f a = f a >> return []


optResult :: MonadIO m => (a -> m (Maybe b)) -> a -> m [b]
optResult f a = liftM Maybe.maybeToList $ f a


process :: (Catch.MonadCatch m, MonadIO m, Functor m)
        => HandlerMap s m -> Message -> StateT s m [Message]
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

