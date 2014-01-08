---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.NodeDefault where

import Data.IORef (IORef)

import qualified Data.IORef                                                 as IORef
import           Flowbox.Batch.Batch                                        (Batch)
import qualified Flowbox.Batch.Handler.NodeDefault                          as BatchND
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.NodeDefault  ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Args        as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Result      as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Args   as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Result as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Args      as SetNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Result    as SetNodeDefault



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.NodeDefault"


nodeDefaults :: IORef Batch -> NodeDefaults.Args -> IO NodeDefaults.Result
nodeDefaults batchHandler (NodeDefaults.Args tnodeID tbc tlibID tprojectID) = do
    loggerIO info "called nodeDefaults"
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    nd <- BatchND.nodeDefaults nodeID bc libID projectID batch
    return $ NodeDefaults.Result $ encode nd


setNodeDefault :: IORef Batch -> SetNodeDefault.Args -> IO SetNodeDefault.Result
setNodeDefault batchHandler (SetNodeDefault.Args tdstPort tvalue tnodeID tbc tlibID tprojectID) = do
    loggerIO info "called setNodeDefault"
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        value     = decodeP tvalue
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchND.setNodeDefault dstPort value nodeID bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return SetNodeDefault.Result


removeNodeDefault :: IORef Batch -> RemoveNodeDefault.Args -> IO RemoveNodeDefault.Result
removeNodeDefault batchHandler (RemoveNodeDefault.Args tdstPort tnodeID tbc tlibID tprojectID) = do
    loggerIO info "called removeNodeDefault"
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchND.removeNodeDefault dstPort nodeID bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return RemoveNodeDefault.Result
