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
import           Control.Monad.Trans.State
import qualified Data.Maybe                as Maybe

import           Flowbox.Bus.Data.Message     (CorrelationID, Message)
import qualified Flowbox.Bus.Data.Message     as Message
import           Flowbox.Bus.Data.Topic       (Topic)
import           Flowbox.Bus.RPC.HandlerMap   (HandlerMap, HandlerMapWithCid)
import qualified Flowbox.Bus.RPC.HandlerMap   as HandlerMap
import qualified Flowbox.Bus.RPC.RPC          as RPC
import           Flowbox.Prelude              hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers as Proto
import           Generated.Proto.Rpc.Response (Response)



logger :: LoggerIO
logger = getLoggerIO $moduleName


singleResult :: MonadIO m => (a -> m b) -> a -> m ([b], [Message])
singleResult f a = do
    b <- f a
    return (return b, [])


singleResultWithCid :: MonadIO m => (CorrelationID -> a -> m b) -> CorrelationID -> a -> m ([b], [Message])
singleResultWithCid f cid a = do
    b <- f cid a
    return (return b, [])

noResult :: MonadIO m => (a -> m ()) -> a -> m ([Response], [Message])
noResult f a = f a >> return ([], [])

-- FIXME: typ malo mowi
optResult :: MonadIO m => (a -> m (Maybe b)) -> a -> m ([b], [Message])
optResult f a = do
    arr <- liftM Maybe.maybeToList $ f a
    return (arr, [])

-- FIXME!!! process i processWithCid - uogolnic
-- Zapytaj o to Piotrka
-- jezeli brak pomyslow, to zawolajcie WD
process :: (Catch.MonadCatch m, MonadIO m, Functor m)
        => HandlerMap s m -> Message -> StateT s m [Message]
process handlerMap msg = HandlerMap.lookupAndCall handlerMap call topic where
    call :: (Catch.MonadCatch m, MonadIO m, Functor m) => HandlerMap.Callback s m
    call mkTopic method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.run $ method args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right (ok, undos) -> map (respond mkTopic) ok ++ undos

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => (Topic -> Topic) -> msg -> Message
    respond mkTopic = Message.mk (mkTopic topic)


processWithCid :: (Catch.MonadCatch m, MonadIO m, Functor m)
               => HandlerMapWithCid s m -> CorrelationID -> Message -> StateT s m [Message]
processWithCid handlerMap cid msg = HandlerMap.lookupAndCallWithCid handlerMap call topic where
    call :: (Catch.MonadCatch m, MonadIO m, Functor m) => HandlerMap.CallbackWithCid s m
    call mkTopic method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.run $ method cid args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right (ok, undos) -> map (respond mkTopic) ok ++ undos

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => (Topic -> Topic) -> msg -> Message
    respond mkTopic = Message.mk (mkTopic topic)
