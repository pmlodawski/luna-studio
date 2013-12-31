
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Transport.TCP.TCP where

import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy             as ByteString
import           Data.Int                         (Int64)
import           Network.Socket                   (Socket)
import qualified Network.Socket                   as Socket
import qualified Network.Socket.ByteString        as SByteString
import qualified Network.Socket.ByteString.Lazy   as SLByteString
import qualified Text.ProtocolBuffers.WireMessage as WireMessage

import Flowbox.Prelude hiding (error)



openSocket :: String -> Int -> Int -> IO Socket
openSocket address port maxConnections = do
    let tcp = 6
    socket <- Socket.socket Socket.AF_INET Socket.Stream tcp
    Socket.setSocketOption socket Socket.ReuseAddr 1
    (Socket.AddrInfo _ _ _ _ sockAddr _):_ <- Socket.getAddrInfo Nothing (Just address) (Just $ show port)
    --serverAddress <- Socket.inet_addr "127.0.0.1"
    --let sockAddr = Socket.SockAddrInet (Socket.PortNum 30521) serverAddress --Socket.iNADDR_ANY
    Socket.bind socket sockAddr -- TODO [PM] pretty code doesn't work ;/
    Socket.listen socket maxConnections
    return socket


sendData :: Socket -> ByteString -> IO ()
sendData socket bytes = SByteString.sendAll socket $ ByteString.toStrict bytes


readData :: Socket -> IO ByteString
readData controlSocket = do
    header <- SLByteString.recv controlSocket 1024
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
            n <- SLByteString.recv controlSocket l
            let stillToRead = l - ByteString.length n
                d = ByteString.append begin n
            if stillToRead == 0
                then return d
                else readRest d stillToRead
