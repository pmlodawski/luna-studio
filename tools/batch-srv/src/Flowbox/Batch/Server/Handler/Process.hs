---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Process where

import Data.IORef (IORef)

import qualified Data.IORef                                     as IORef
import           Flowbox.Batch.Batch                            (Batch)
import qualified Flowbox.Batch.Handler.Process                  as BatchP
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Process.Processes.Args   as Processes
import qualified Generated.Proto.Batch.Process.Processes.Result as Processes
import qualified Generated.Proto.Batch.Process.Terminate.Args   as Terminate
import qualified Generated.Proto.Batch.Process.Terminate.Result as Terminate



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Process"

-------- public api -------------------------------------------------

processes :: IORef Batch -> Processes.Args -> IO Processes.Result
processes batchHandler (Processes.Args tprojectID) = do
    loggerIO info "called processes"
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    processIDs <- BatchP.processes projectID batch
    return $ Processes.Result $ encodeListP processIDs


terminate :: IORef Batch -> Terminate.Args -> IO Terminate.Result
terminate batchHandler (Terminate.Args tprocessID tprojectID) = do
    loggerIO info "called terminate"
    let processID = decodeP tprocessID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchP.terminate processID projectID batch
    IORef.writeIORef batchHandler newBatch
    return Terminate.Result

