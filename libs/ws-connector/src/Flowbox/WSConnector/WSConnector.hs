---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2015
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.WSConnector.WSConnector where

import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Monad         (forever)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets    as WS
import qualified System.ZMQ4.Monadic   as ZMQ

import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Flag         as Flag
import qualified Flowbox.Bus.Data.Message      as Message
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import qualified Flowbox.Bus.EndPoint          as EP
import qualified Flowbox.Config.Config         as Config
import qualified FlowboxData.Config.Config     as FD
import qualified Flowbox.Network.WebSocket.Connector.Config.Config as WS
import Flowbox.Control.Error
import Flowbox.Prelude


fromWeb :: WS.Connection -> Bus ()
fromWeb conn = do
    forever $ do
        webMessage <- liftIO $ do WS.receiveData conn
        Bus.sendByteString webMessage

toWeb :: WS.Connection -> Bus ()
toWeb  conn = do
    Bus.subscribe mempty
    forever $ do
        messageBus <- Bus.receiveByteString
        liftIO $ do WS.sendTextData conn messageBus


runBus :: EP.BusEndPoints -> Bus a -> IO (Either Bus.Error a)
runBus e b = ZMQ.runZMQ $ Bus.runBus e b

application ::  Int -> EP.BusEndPoints -> WS.ServerApp
application pingTime busEndPoints pending = do
    conn <- WS.acceptRequest pending

    WS.forkPingThread conn pingTime

    _ <- forkIO $ eitherToM' $ runBus busEndPoints $ fromWeb conn
    void $ runBus busEndPoints $ toWeb conn


run :: IO ()
run = do
    busEndPoints <- EP.clientFromConfig <$> Config.load
    config <- WS.readWebsocketConfig <$> FD.load

    WS.runServer (config ^. WS.host) (config ^. WS.port) $ application (config ^. WS.pingTime) busEndPoints