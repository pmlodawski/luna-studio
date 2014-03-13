---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Bus.Client where

import Control.Monad       (forever)
import Control.Monad.Trans

import           Control.Monad.Trans.Either
import           Flowbox.Bus.Bus            (Bus)
import qualified Flowbox.Bus.Bus            as Bus
import qualified Flowbox.Bus.Env            as Env
import           Flowbox.Bus.Message        (Message)
import qualified Flowbox.Bus.Message        as Message
import           Flowbox.Bus.MessageFrame   (MessageFrame (MessageFrame))
import           Flowbox.Bus.Topic          (Topic)
import           Flowbox.Prelude            hiding (error)
import           Flowbox.System.Log.Logger




logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Client"


run :: Env.BusEndPoints -> Topic -> (Message -> IO Message) -> IO (Either String ())
run endPoints topic process = Bus.runBus (handleLoop topic process) endPoints


handleLoop :: Topic -> (Message -> IO Message) -> Bus ()
handleLoop topic process = do
    Bus.subscribe topic
    _ <- forever $ handle process
    return ()


handle :: (Message -> IO Message) -> Bus ()
handle process = do
    request <- Bus.receive
    r <- lift $ hoistEither request
    case r of
        (MessageFrame msg crlID _) -> do
            liftIO $ logger debug $ "Received request: " ++ Message.topic msg
            response <- liftIO $ process msg
            Bus.reply crlID response

