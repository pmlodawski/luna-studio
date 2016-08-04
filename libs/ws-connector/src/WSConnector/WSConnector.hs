{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConnector where

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger

import           Control.Concurrent                    (forkIO)
import           Control.Concurrent.STM                (STM, atomically)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad                         (forever)
import qualified Data.ByteString                       as ByteString
import qualified Network.WebSockets                    as WS

import           Flowbox.Bus.EndPoint                  (BusEndPoints)

import           WSConnector.Data.WSFrame      (WSFrame (..), deserializeFrame, messages, serializeFrame)
import           WSConnector.Data.WSMessage    (ControlCode (..), WSMessage (..))
import qualified WSConnector.Workers.BusWorker as BusWorker
import qualified WSConnector.WSConfig          as WSConfig

logger :: LoggerIO
logger = getLoggerIO $moduleName

handleDisconnect :: WS.ConnectionException -> IO ()
handleDisconnect _ = logger info "User disconnected"

getFreshClientId :: TVar Int -> TVar Int -> STM Int
getFreshClientId clientCounter currentClient = do
    modifyTVar clientCounter (+ 1)
    newId <- readTVar clientCounter
    writeTVar currentClient newId
    return newId

application :: TVar Int -> TVar Int -> Int -> TChan WSMessage -> TChan WSMessage -> WS.ServerApp
application clientCounter currentClient pingTime toBusChan fromBusChan pending = do
    newId <- atomically $ getFreshClientId clientCounter currentClient
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn pingTime

    let welcomeMessage = serializeFrame $ WSFrame [ControlMessage Welcome]
    WS.sendTextData conn welcomeMessage

    fromBusListenChan <- atomically $ dupTChan fromBusChan

    forkIO $ fromWeb newId clientCounter conn toBusChan
    toWeb conn fromBusListenChan

isActive :: Int -> TVar Int -> STM Bool
isActive clientId currentClient = return True
-- do
--     currentClientId <- readTVar currentClient
--     return $ clientId == currentClientId

whileActive :: Int -> TVar Int -> IO () -> IO ()
whileActive clientId currentClient action = do
    active <- atomically $ isActive clientId currentClient
    if active
        then do action
                whileActive clientId currentClient action
        else return ()

fromWebLoop :: Int -> TVar Int -> WS.Connection -> TChan WSMessage -> IO ()
fromWebLoop clientId currentClient conn chan = whileActive clientId currentClient $ do
    webMessage <- WS.receiveData conn
    let frame = deserializeFrame webMessage
    active <- atomically $ isActive clientId currentClient
    if active
        then atomically $ mapM_ (writeTChan chan) $ frame ^. messages
        else return ()

fromWeb :: Int -> TVar Int -> WS.Connection -> TChan WSMessage -> IO ()
fromWeb clientId currentClient conn chan = do
    flip catch handleDisconnect $ fromWebLoop clientId currentClient conn chan
    let takeoverMessage = serializeFrame $ WSFrame [ControlMessage ConnectionTakeover]
    WS.sendTextData conn takeoverMessage
    WS.sendClose conn takeoverMessage

toWeb :: WS.Connection -> TChan WSMessage -> IO ()
toWeb conn chan = flip catch handleDisconnect $ forever $ do
    msg <- atomically $ readTChan chan
    let webMessage = serializeFrame $ WSFrame [msg]
    WS.sendTextData conn webMessage

run :: BusEndPoints -> WSConfig.Config -> IO ()
run busEndPoints config = do
    toBusChan       <- atomically newTChan
    fromBusChan     <- atomically newBroadcastTChan
    clientCounter   <- atomically $ newTVar 0
    currentClient   <- atomically $ newTVar 0

    BusWorker.start busEndPoints fromBusChan toBusChan

    WS.runServer (config ^. WSConfig.host) (config ^. WSConfig.port) $ application clientCounter currentClient (config ^. WSConfig.pingTime) toBusChan fromBusChan
