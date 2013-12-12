---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.NodeDefault where

import Data.IORef (IORef)

import           Flowbox.Batch.Batch                                        (Batch)
import qualified Flowbox.Batch.Handler.NodeDefault                          as BatchND
import           Flowbox.Control.Error
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


nodeDefaults :: IORef Batch -> NodeDefaults.Args -> Script NodeDefaults.Result
nodeDefaults batchHandler (NodeDefaults.Args tnodeID tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called nodeDefaults"
    bc <- tryRight $ decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    nd <- scriptIO $ BatchND.nodeDefaults nodeID bc libID projectID batch
    return $ NodeDefaults.Result $ encode nd


setNodeDefault :: IORef Batch -> SetNodeDefault.Args -> Script SetNodeDefault.Result
setNodeDefault batchHandler (SetNodeDefault.Args tdstPort tvalue tnodeID tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called setNodeDefault"
    bc      <- tryRight $ decode tbc
    let dstPort   = decodeP tdstPort
        value     = decodeP tvalue
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchND.setNodeDefault dstPort value nodeID bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return SetNodeDefault.Result


removeNodeDefault :: IORef Batch -> RemoveNodeDefault.Args -> Script RemoveNodeDefault.Result
removeNodeDefault batchHandler (RemoveNodeDefault.Args tdstPort tnodeID tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called removeNodeDefault"
    bc      <- tryRight $ decode tbc
    let dstPort   = decodeP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchND.removeNodeDefault dstPort nodeID bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return RemoveNodeDefault.Result
