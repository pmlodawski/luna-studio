
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude                                    hiding (error)
import           Control.Monad                                (when)
import qualified Control.Concurrent                         as Concurrent
import qualified Control.Concurrent.MVar                    as MVar
import           Control.Concurrent.MVar                      (MVar)
import qualified Control.Exception                          as Exception
import           Data.Text.Lazy                               (pack)
import qualified System.Exit                                as Exit

import qualified Batch                                      as TBatch
import           Batch_Iface                                  
import           Thrift.Transport.Handle                      ()
import qualified Thrift.Protocol.Binary                     as TProtocol
import           Thrift.Protocol.Binary                       (Protocol)
import           Thrift.Transport                             (Transport)

import qualified Flowbox.Batch.Server.Handlers.BatchHandler as BatchHandler
import qualified Flowbox.Batch.Server.Server                as Server
import           Flowbox.System.Log.Logger                    



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server"


address :: String
address = "127.0.0.1"


port :: Int
port = 30521


main :: IO ()
main = do
    rootLogger setLevel DEBUG
    quitmutex <- MVar.newEmptyMVar
    _ <- Concurrent.forkIO $ Exception.handle 
        (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                               MVar.putMVar quitmutex True) 
        (serve quitmutex)
    waitForQuit quitmutex


serve :: MVar Bool -> IO ()
serve quitmutex = do
    handler <- BatchHandler.empty
    loggerIO info "Starting the server"
    _ <- Server.runSingleConnectionServer Server.accepter handler (processCommand quitmutex) address port
    return ()


processCommand :: (Protocol iprot, Protocol oprot, Transport itransp, Transport otransp, Batch_Iface batch)
         => MVar Bool -> batch -> (iprot itransp, oprot otransp) -> IO Bool
processCommand quitmutex handler (iprot, oprot) = do
    (name, typ, seqid) <- TProtocol.readMessageBegin iprot
    TBatch.proc_ handler (iprot,oprot) (name,typ,seqid)
    when (name == pack "shutdown") (MVar.putMVar quitmutex True)
    return True


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    Concurrent.threadDelay 1000000
    loggerIO warning "shutting down..."
    Exit.exitSuccess
