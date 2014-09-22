---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}

module Flowbox.Bus.Server where

import Control.Monad             (forever)
import "mmorph" Control.Monad.Morph
import Control.Monad.Trans
import Control.Monad.Trans.State

import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT (BusT))
import qualified Flowbox.Bus.BusT              as BusT
import qualified Flowbox.Bus.Data.Flag         as Flag
import           Flowbox.Bus.Data.Message      (Message)
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic        (Topic)
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Server"


run :: BusEndPoints -> [Topic] -> (Message -> IO [Message]) -> IO (Either Bus.Error ())
run endPoints topics process = Bus.runBus endPoints $ handleLoop topics process


handleLoop :: [Topic] -> (Message -> IO [Message]) -> Bus ()
handleLoop topics process = do
    mapM_ Bus.subscribe topics
    _ <- forever $ handle process
    return ()


handle :: (Message -> IO [Message]) -> Bus ()
handle process = do
    (MessageFrame msg crlID _ _) <- Bus.receive
    liftIO $ logger debug $ "Received request: " ++ (msg ^. Message.topic)
    response <- liftIO $ process msg
    unless (null response) $ do
        mapM_ (Bus.reply crlID Flag.Disable) (init response)
        Bus.reply crlID Flag.Enable $ last response


runState :: BusEndPoints -> [Topic] -> s -> (Message -> StateT s IO [Message]) -> IO (Either Bus.Error ())
runState endPoints topics s process = Bus.runBus endPoints $ handleLoopState topics s process


handleLoopState :: [Topic] -> s -> (Message -> StateT s IO [Message]) -> Bus ()
handleLoopState topics s process = do
    mapM_ Bus.subscribe topics
    _ <- BusT.runBusT $ runStateT (forever $ handleState process) s
    return ()


handleState :: (Message -> StateT s IO [Message]) -> StateT s BusT ()
handleState process = do
    (MessageFrame msg crlID _ _) <- lift $ BusT Bus.receive
    liftIO $ logger debug $ "Received request: " ++ (msg ^. Message.topic)
    response <- hoist liftIO $ process msg
    lift $ BusT $ unless (null response) $ do
        mapM_ (Bus.reply crlID Flag.Disable) (init response)
        Bus.reply crlID Flag.Enable $ last response
