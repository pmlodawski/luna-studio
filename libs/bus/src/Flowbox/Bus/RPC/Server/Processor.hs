---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Server.Processor where

import Control.Monad          (liftM)
import Control.Monad.IO.Class (MonadIO)

import           Flowbox.Bus.Data.Message     (Message)
import qualified Flowbox.Bus.Data.Message     as Message
import           Flowbox.Bus.RPC.HandlerMap   (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap   as HandlerMap
import qualified Flowbox.Bus.RPC.RPC          as RPC
import           Flowbox.Prelude              hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Server.Processor"


singleResult :: MonadIO m => (a -> m b) -> a -> m [b]
singleResult f a = liftM mkList $ f a


-- FIXME [PM] : Code duplication


process :: HandlerMap IO -> Message -> IO [Message]
process handlerMap msg = HandlerMap.lookupAndCall handlerMap call topic where
    call type_ method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.run $ method args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right ok -> map (respond type_) ok

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => String -> msg -> Message
    respond = Message.mkResponse topic


processLifted :: MonadIO m => HandlerMap m -> Message -> m [Message]
processLifted handlerMap msg = HandlerMap.lookupAndCall handlerMap call topic where
    call type_ method = case Proto.messageGet' $ msg ^. Message.message of
        Left err   -> do logger error err
                         return $ Message.mkError topic err
        Right args -> do results <- RPC.runLifted $ method args
                         return $ case results of
                            Left err -> Message.mkError topic err
                            Right ok -> map (respond type_) ok

    topic = msg ^. Message.topic

    respond :: Proto.Serializable msg => String -> msg -> Message
    respond = Message.mkResponse topic

