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
import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.Data.Message      (Message)
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic        (Topic)
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Client"


run :: BusEndPoints -> [Topic] -> (Message -> IO [Message]) -> IO (Either String ())
run endPoints topics process = Bus.runBus endPoints $ handleLoop topics process


handleLoop :: [Topic] -> (Message -> IO [Message]) -> Bus ()
handleLoop topics process = do
    mapM_ Bus.subscribe topics
    _ <- forever $ handle process
    return ()


handle :: (Message -> IO [Message]) -> Bus ()
handle process = do
    request <- Bus.receive
    (MessageFrame msg crlID _ _) <- lift $ hoistEither request
    liftIO $ logger debug $ "Received request: " ++ Message.topic msg
    response <- liftIO $ process msg
    if not $ null response
        then do mapM_ (Bus.reply crlID False) (init response)
                Bus.reply crlID True $ last response
        else return ()
