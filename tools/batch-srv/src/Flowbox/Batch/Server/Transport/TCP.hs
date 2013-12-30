
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Server.Transport.TCP where

import qualified Control.Exception                as Exception
import           Control.Monad                    (forever)
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy             as ByteString
import           Data.Int                         (Int64)
import qualified Network.Socket                   as Socket
import qualified Network.Socket.ByteString        as SByteString
import qualified Network.Socket.ByteString.Lazy   as SLByteString
import qualified Text.ProtocolBuffers.WireMessage as WireMessage

import           Flowbox.Batch.Server.Cmd             (Cmd)
import qualified Flowbox.Batch.Server.Cmd             as Cmd
import           Flowbox.Batch.Server.Handler.Handler (Handler)
import qualified Flowbox.Batch.Server.Processor       as Processor
import           Flowbox.Prelude                      hiding (error)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.TCP"


--TODO [PM] : Refactor needed
--TODO [PM] : Magic constants
--TODO [PM] : Cleanup needed


serve :: Handler h => Cmd -> h -> IO ()
serve cmd handler = Socket.withSocketsDo $ do
    let address = Cmd.address cmd
        port    = Cmd.port cmd
        tcp = 6
        maxConnections = 1

    socket  <- Socket.socket Socket.AF_INET Socket.Stream tcp
    Socket.setSocketOption socket Socket.ReuseAddr 1
    (Socket.AddrInfo _ _ _ _ sockAddr _):_ <- Socket.getAddrInfo Nothing (Just address) (Just $ show port)
    --serverAddress <- Socket.inet_addr "127.0.0.1"
    --let sockAddr = Socket.SockAddrInet (Socket.PortNum 30521) serverAddress --Socket.iNADDR_ANY
    Socket.bind socket sockAddr -- TODO [PM] pretty code doesn't work ;/
    Socket.listen socket maxConnections

    if Cmd.shutdownWithClient cmd
        then acceptAndHandle socket handler
        else forever $ Exception.handle
            (\(e :: Exception.SomeException) -> loggerIO critical $ "Connection to client lost: " ++ show e)
            (acceptAndHandle socket handler)


acceptAndHandle :: Handler h => Socket.Socket -> h -> IO ()
acceptAndHandle socket handler = do
    (client, clientSockAddr) <- Socket.accept socket
    loggerIO info $ "Accepted connection from " ++ (show clientSockAddr)
    forever $ handleCall client handler


handleCall :: (Handler handler) => Socket.Socket -> handler -> IO ()
handleCall socket handler = do
    loggerIO debug "handleCall: started"
    encoded_request  <- readData socket
    encoded_response <- Processor.process handler encoded_request
    loggerIO debug "handleCall: processing done"
    SByteString.sendAll socket $ ByteString.toStrict encoded_response
    loggerIO debug "handleCall: reply sent"


readData :: Socket.Socket -> IO ByteString
readData socket = do
    header <- SLByteString.recv socket 1024
    (size, _) <- case WireMessage.runGetOnLazy WireMessage.getVarInt header of
                    Left   m     -> fail m
                    Right (s, r) -> return (s, r)
    let toRead = size + 4 - (ByteString.length header)
    if toRead > 0
        then readRest header toRead
        else return header
    where
        readRest :: ByteString -> Int64 -> IO ByteString
        readRest begin l = do
            n <- SLByteString.recv socket l
            let stillToRead = l - ByteString.length n
                d = ByteString.append begin n
            if stillToRead == 0
                then return d
                else readRest d stillToRead
