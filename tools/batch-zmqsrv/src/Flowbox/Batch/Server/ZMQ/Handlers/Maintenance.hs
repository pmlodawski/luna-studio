---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.ZMQ.Handlers.Maintenance (
    ping,
    dump,
    shutdown,
    initialize,
) where

import qualified Control.Concurrent.MVar                                  as MVar
import           Control.Concurrent.MVar                                    (MVar)
import           Data.IORef                                                 (IORef)
import qualified System.ZMQ3.Monadic                                      as ZMQ3

import           Flowbox.Prelude                                            
import           Flowbox.Batch.Batch                                        (Batch)
import qualified Flowbox.Batch.Handlers.Maintenance                       as BatchM
import           Flowbox.Batch.Server.ZMQ.Handlers.Common                   (zmqRunScript)
import           Flowbox.Control.Error                                      
import           Flowbox.System.Log.Logger                                  
import qualified Generated.ServerApi.Server.Maintenance.Initialize.Args   as Initialize
import qualified Generated.ServerApi.Server.Maintenance.Initialize.Result as Initialize
import qualified Generated.ServerApi.Server.Maintenance.Ping.Args         as Ping
import qualified Generated.ServerApi.Server.Maintenance.Ping.Result       as Ping
import qualified Generated.ServerApi.Server.Maintenance.Dump.Args         as Dump
import qualified Generated.ServerApi.Server.Maintenance.Dump.Result       as Dump
import qualified Generated.ServerApi.Server.Maintenance.Shutdown.Args     as Shutdown
import qualified Generated.ServerApi.Server.Maintenance.Shutdown.Result   as Shutdown



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Handlers.Maintenance"


------ public api -------------------------------------------------
initialize :: IORef Batch -> Initialize.Args -> ZMQ3.ZMQ z Initialize.Result
initialize batchHandler _ = zmqRunScript $ do 
    scriptIO $ loggerIO info "called initialize"
    batch <- tryReadIORef batchHandler
    scriptIO $ BatchM.initialize batch
    return Initialize.Result


ping :: IORef Batch -> Ping.Args -> ZMQ3.ZMQ z Ping.Result
ping _ _ = zmqRunScript $ do
    loggerIO info "called ping"
    return Ping.Result


dump :: IORef Batch -> Dump.Args -> ZMQ3.ZMQ z Dump.Result
dump batchHandler _ = zmqRunScript $ do
    loggerIO info "called ping"
    batch <- tryReadIORef batchHandler
    scriptIO $ print batch
    return Dump.Result


shutdown :: MVar Bool -> Shutdown.Args -> ZMQ3.ZMQ z Shutdown.Result
shutdown quitMutex _ = zmqRunScript $ do
    loggerIO info "called shutdown"
    scriptIO $ MVar.putMVar quitMutex True
    return Shutdown.Result


