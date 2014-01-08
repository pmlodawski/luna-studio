---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Maintenance (
    ping,
    dump,
    shutdown,
    initialize,
) where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy    as ByteString
import           Data.IORef              (IORef)

import qualified Data.IORef                                          as IORef
import           Flowbox.Batch.Batch                                 (Batch)
import qualified Flowbox.Batch.Handler.Maintenance                   as BatchM
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.Batch.Maintenance.Dump.Args         as Dump
import qualified Generated.Proto.Batch.Maintenance.Dump.Result       as Dump
import qualified Generated.Proto.Batch.Maintenance.Initialize.Args   as Initialize
import qualified Generated.Proto.Batch.Maintenance.Initialize.Result as Initialize
import qualified Generated.Proto.Batch.Maintenance.Ping.Args         as Ping
import qualified Generated.Proto.Batch.Maintenance.Ping.Result       as Ping
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Args     as Shutdown
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Result   as Shutdown



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.Maintenance"


------ public api -------------------------------------------------
initialize :: IORef Batch -> Initialize.Args -> IO Initialize.Result
initialize batchHandler _ = do
    loggerIO info "called initialize"
    batch <- IORef.readIORef batchHandler
    BatchM.initialize batch
    return Initialize.Result


ping :: IORef Batch -> Ping.Args -> IO Ping.Result
ping _ (Ping.Args mdata) = do
    loggerIO info "called ping"
    case mdata of
        Nothing -> return ()
        Just d  -> loggerIO info $ "Received " ++ (show $ ByteString.length d) ++ " bytes"
    return $ Ping.Result Nothing


dump :: IORef Batch -> Dump.Args -> IO Dump.Result
dump batchHandler _ = do
    loggerIO info "called dump"
    batch <- IORef.readIORef batchHandler
    print batch
    return Dump.Result


shutdown :: MVar Bool -> Shutdown.Args -> IO Shutdown.Result
shutdown quitMutex _ = do
    loggerIO info "called shutdown"
    MVar.putMVar quitMutex True
    return Shutdown.Result


