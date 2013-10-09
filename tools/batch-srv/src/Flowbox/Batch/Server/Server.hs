
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Server.Server ( 
    accepter,
    runSingleConnectionServer,
) where

import           Control.Monad                  (forever, when)
import qualified Control.Exception            as Exception
import qualified Network                      as Network
import qualified Network.Socket               as Socket
import qualified System.IO                    as IO
import           Thrift.Transport.Handle        ()
import qualified Thrift.Protocol.Binary       as TProtocol
import           Thrift.Protocol.Binary         (Protocol)
import           Thrift.Transport               (Transport)

import           Flowbox.Prelude                
import qualified Flowbox.Batch.Server.CmdArgs as CmdArgs
import           Flowbox.Batch.Server.CmdArgs   (CmdArgs)
import           Flowbox.System.Log.Logger      



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server"


accepter :: Network.Socket -> IO (TProtocol.BinaryProtocol IO.Handle,
                                  TProtocol.BinaryProtocol IO.Handle)
accepter s = do
    (h, addr, p) <- Network.accept s
    loggerIO info $ "Accepted connection from " ++ addr ++ " : " ++ (show p)
    return (TProtocol.BinaryProtocol h, TProtocol.BinaryProtocol h)


runSingleConnectionServer :: (Transport t, Protocol i, Protocol o)
                  => (Network.Socket -> IO (i t, o t))
                  -> h -> (h -> (i t, o t) -> IO Bool) -> CmdArgs -> IO ()
runSingleConnectionServer accepter_ hand proc_ cmd = Socket.withSocketsDo $ do
    let tcp = 6
        maxConnections = 1

    serverSocket  <- Socket.socket Socket.AF_INET Socket.Stream tcp
    --serverAddress <- Socket.inet_addr "127.0.0.1"
    Socket.setSocketOption serverSocket Socket.ReuseAddr 1
    (Socket.AddrInfo _ _ _ _ sockAddr _):_ <- Socket.getAddrInfo Nothing (Just $ CmdArgs.address cmd) (Just $ CmdArgs.port cmd)
    --let sockAddr = Socket.SockAddrInet (Socket.PortNum 30521) serverAddress --Socket.iNADDR_ANY
    Socket.bindSocket serverSocket sockAddr -- TODO [PM] pretty code doesn't work ;/
    Socket.listen serverSocket maxConnections
    if CmdArgs.shutdownWithClient cmd
        then singleAccept (accepter_ serverSocket) (proc_ hand)
        else singleAcceptLoop (accepter_ serverSocket) (proc_ hand)


singleAcceptLoop :: IO t -> (t -> IO Bool) -> IO ()
singleAcceptLoop accepter_ proc_ = forever $ do
    ps <- accepter_
    Exception.handle 
        (\(e :: Exception.SomeException) -> loggerIO critical $ "Connection to client lost: " ++ show e) 
        (loop $ proc_ ps)


singleAccept :: IO t -> (t -> IO Bool) -> IO ()
singleAccept accepter_ proc_ = forever $ do
    ps <- accepter_
    loop $ proc_ ps


loop :: IO Bool -> IO ()
loop m = do continue <- m
            when continue (loop m)