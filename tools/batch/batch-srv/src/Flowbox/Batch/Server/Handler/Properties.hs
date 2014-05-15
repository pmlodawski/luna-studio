---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Properties where

import Data.IORef (IORef)

import qualified Data.IORef                                               as IORef
import           Flowbox.Batch.Batch                                      (Batch)
import qualified Flowbox.Batch.Handler.Properties                         as BatchP
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Properties.GetProperties.Args      as GetProperties
import qualified Generated.Proto.Batch.Properties.GetProperties.Result    as GetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Args      as SetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Result    as SetProperties



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Properties"


getProperties :: IORef Batch -> GetProperties.Args -> IO GetProperties.Result
getProperties batchHandler (GetProperties.Args tnodeID tlibID tprojectID) = do
    loggerIO info "called getProperties"
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    properties <- BatchP.getProperties nodeID libID projectID batch
    return $ GetProperties.Result $ encode properties


setProperties :: IORef Batch -> SetProperties.Args -> IO SetProperties.Result
setProperties batchHandler (SetProperties.Args tproperties tnodeID tlibID tprojectID) = do
    loggerIO info "called setProperties"
    properties <- decode tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchP.setProperties properties nodeID libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return SetProperties.Result
