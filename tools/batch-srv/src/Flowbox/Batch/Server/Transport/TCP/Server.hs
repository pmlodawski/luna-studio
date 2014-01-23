
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Server.Transport.TCP.Server where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception       as Exception
import           Control.Monad           (forM_, forever)
import           Network.Socket          (Socket)
import qualified Network.Socket          as Socket

import           Flowbox.Batch.Server.Cmd                       (Cmd)
import qualified Flowbox.Batch.Server.Cmd                       as Cmd
import           Flowbox.Batch.Server.Handler.Handler           (Handler)
import qualified Flowbox.Batch.Server.Processor                 as Processor
import qualified Flowbox.Batch.Server.Transport.TCP.TCP         as TCP
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.TCP"


serve :: Handler h => Cmd -> h -> IO ()
serve cmd handler = Socket.withSocketsDo $ do
    let address     = Cmd.address cmd
        controlPort = Cmd.port cmd
        notifyPort  = controlPort + 1
        maxConnections = 1

    controlServerSocket <- TCP.openSocket address controlPort maxConnections
    notifyServerSocket  <- TCP.openSocket address notifyPort  maxConnections

    if Cmd.shutdownWithClient cmd
        then acceptAndHandle controlServerSocket notifyServerSocket handler
        else forever $ Exception.handle
            (\(e :: Exception.SomeException) -> loggerIO critical $ "Connection to client lost: " ++ show e)
            (acceptAndHandle controlServerSocket notifyServerSocket handler)


acceptAndHandle :: Handler h => Socket -> Socket -> h -> IO ()
acceptAndHandle controlServerSocket notifyServerSocket handler = do
    loggerIO info $ "Waiting for control connection."
    (controlSocket, controlSocketAddr) <- Socket.accept controlServerSocket
    loggerIO info $ "Control socket connected. Waiting for notify connection."
    (notifySocket , notifySocketAddr ) <- Socket.accept notifyServerSocket
    loggerIO info $ "Accepted connection from " ++ (show controlSocketAddr) ++ " / " ++ (show notifySocketAddr)
    mnotifySocket <- MVar.newMVar notifySocket
    forM_ [0..] $ handleCall controlSocket mnotifySocket handler


handleCall :: Handler h => Socket -> MVar Socket -> h -> Int -> IO ()
handleCall controlSocket notifySocket handler requestID = do
    encoded_request  <- TCP.readData controlSocket
    encoded_response <- Processor.process notifySocket handler encoded_request $ encodeP requestID
    TCP.sendData controlSocket encoded_response
