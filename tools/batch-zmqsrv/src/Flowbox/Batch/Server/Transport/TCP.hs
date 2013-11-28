
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Transport.TCP where

import           Control.Monad                           (forever)
import qualified Network.Socket                        as Socket
import qualified Network.Socket.ByteString.Lazy        as SLByteString
import qualified Data.ByteString.Lazy                  as ByteString
import           Data.ByteString.Lazy                    (ByteString)
import           Data.Int                                (Int64)
import           Flowbox.Prelude                       hiding (error)
import qualified Flowbox.Batch.Server.Processor        as Processor
import           Flowbox.Batch.Server.Handlers.Handler   (Handler)
import           Flowbox.System.Log.Logger               
import qualified Text.ProtocolBuffers.WireMessage      as WireMessage
import qualified Network.Socket.ByteString as SByteString


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.TCP"


--TODO [PM] : Refactor needed
--TODO [PM] : Magic constants
--TODO [PM] : Cleanup needed


serve :: Handler h => String -> Int -> h -> IO ()
serve address port handler = Socket.withSocketsDo $ do
    let tcp = 6
        maxConnections = 1
    socket  <- Socket.socket Socket.AF_INET Socket.Stream tcp
    --serverAddress <- Socket.inet_addr "127.0.0.1"
    Socket.setSocketOption socket Socket.ReuseAddr 1
    (Socket.AddrInfo _ _ _ _ sockAddr _):_ <- Socket.getAddrInfo Nothing (Just address) (Just $ show port)
    --let sockAddr = Socket.SockAddrInet (Socket.PortNum 30521) serverAddress --Socket.iNADDR_ANY
    Socket.bindSocket socket sockAddr -- TODO [PM] pretty code doesn't work ;/
    Socket.listen socket maxConnections
    (client, clientSockAddr) <- Socket.accept socket
    loggerIO info $ "Accepted connection from " ++ (show clientSockAddr)
    --IO.hSetBinaryMode h True
    forever $ handleCall client handler

    --if Cmd.shutdownWithClient cmd
    --    then singleAccept (accepter_ socket) (proc_ hand)
    --    else singleAcceptLoop (accepter_ socket) (proc_ hand)
--    socket <- ZMQ3.socket ZMQ3.Rep
--    ZMQ3.bind socket (address ++ ":" ++ show port)
    --runSingleConnectionServer accepter 


handleCall :: (Handler handler) => Socket.Socket -> handler -> IO ()
handleCall socket handler = do
    loggerIO debug "handleCall: started"       
   
    encoded_request <- readData socket 
    --let (x :: Either String (Request, ByteString.ByteString) ) = Proto.messageGet encoded_request
    --let x = case Proto.messageWithLengthGet encoded_request of
    --            Left m -> Left m
    --            Right (r :: Request, _) -> Right r
    --print x
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
