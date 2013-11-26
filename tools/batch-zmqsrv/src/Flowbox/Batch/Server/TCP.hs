
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.Batch.Server.TCP where

import           Control.Applicative                     
import           Control.Monad                           (forever)
import qualified Data.ByteString.Lazy                  as ByteString
import qualified System.ZMQ3.Monadic                   as ZMQ3

import           Flowbox.Prelude                       hiding (error)
import qualified Flowbox.Batch.Server.Processor        as Processor
import           Flowbox.Batch.Server.Handlers.Handler   (Handler)
import           Flowbox.System.Log.Logger               
import qualified Network                               as Network
import qualified Network.Socket                        as Socket
import qualified System.IO                             as IO
import qualified Text.ProtocolBuffers                                as Proto
import qualified Text.ProtocolBuffers.Basic                          as Proto
import qualified Generated.Proto.Batch.Request                       as Request
import           Generated.Proto.Batch.Request                         (Request)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.TCP"


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
    (h, addr, p) <- Network.accept socket
    loggerIO info $ "Accepted connection from " ++ addr ++ " : " ++ (show p)
    forever $ handleCall h handler

    --if Cmd.shutdownWithClient cmd
    --    then singleAccept (accepter_ socket) (proc_ hand)
    --    else singleAcceptLoop (accepter_ socket) (proc_ hand)
--    socket <- ZMQ3.socket ZMQ3.Rep
--    ZMQ3.bind socket (address ++ ":" ++ show port)
    --runSingleConnectionServer accepter 


--runSingleConnectionServer :: (Transport t, Protocol i, Protocol o)
--                  => (Network.Socket -> IO (i t, o t))
--                  -> h -> (h -> (i t, o t) -> IO Bool) -> Cmd -> IO ()
--runSingleConnectionServer hand proc_ cmd = S


handleCall :: (Handler handler) => IO.Handle -> handler -> IO ()
handleCall handle handler = do
    loggerIO debug "handleCall: started"
    encoded_request  <- ByteString.hGetContents handle
    loggerIO debug $ "handleCall: received request "-- ++ (show $ ByteString.length encoded_request)
    --let (x :: Either String (Request, ByteString.ByteString) ) = Proto.messageGet encoded_request
    let Right (x :: Request, _) = Proto.messageGet encoded_request
    print x
    encoded_response <- Processor.process handler encoded_request
    loggerIO debug "handleCall: processing done"
    ByteString.hPut handle encoded_response
    loggerIO debug "handleCall: reply sent"


--handleCall :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
--handleCall socket handler = do
--    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
--    encoded_response <- ZMQ3.liftIO $ ByteString.toStrict <$> Processor.process handler encoded_request
--    ZMQ3.send socket [] $ encoded_response
