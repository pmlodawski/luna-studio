---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.Batch.Server.Handler.Graph where

import Data.IORef (IORef)

import           Flowbox.Batch.Batch                                 (Batch)
import qualified Flowbox.Batch.Handler.Graph                         as BatchG
import           Flowbox.Control.Error
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Graph ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Graph.AddNode.Args            as AddNode
import qualified Generated.Proto.Batch.Graph.AddNode.Result          as AddNode
import qualified Generated.Proto.Batch.Graph.Connect.Args            as Connect
import qualified Generated.Proto.Batch.Graph.Connect.Result          as Connect
import qualified Generated.Proto.Batch.Graph.Disconnect.Args         as Disconnect
import qualified Generated.Proto.Batch.Graph.Disconnect.Result       as Disconnect
import qualified Generated.Proto.Batch.Graph.NodeByID.Args           as NodeByID
import qualified Generated.Proto.Batch.Graph.NodeByID.Result         as NodeByID
import qualified Generated.Proto.Batch.Graph.NodesGraph.Args         as NodesGraph
import qualified Generated.Proto.Batch.Graph.NodesGraph.Result       as NodesGraph
import qualified Generated.Proto.Batch.Graph.RemoveNode.Args         as RemoveNode
import qualified Generated.Proto.Batch.Graph.RemoveNode.Result       as RemoveNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Args         as UpdateNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Result       as UpdateNode



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Graph"


nodesGraph :: IORef Batch -> NodesGraph.Args -> Script NodesGraph.Result
nodesGraph batchHandler (NodesGraph.Args tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called nodesGraph"
    bc <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    ng <- scriptIO $ BatchG.nodesGraph bc libID projectID batch
    return $ NodesGraph.Result $ encode ng


nodeByID :: IORef Batch -> NodeByID.Args -> Script NodeByID.Result
nodeByID batchHandler (NodeByID.Args tnodeID tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called nodeByID"
    bc <- tryRight $ decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    node  <- scriptIO $ BatchG.nodeByID nodeID bc libID projectID batch
    return $ NodeByID.Result $ encode (nodeID, node)


addNode :: IORef Batch -> AddNode.Args -> Script AddNode.Result
addNode batchHandler (AddNode.Args tnode tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called addNode"
    bc <- tryRight $ decode tbc
    (_ :: Int, node) <- tryRight $ decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    (newBatch, newNodeID) <- scriptIO $ BatchG.addNode node bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return $ AddNode.Result $ encode (newNodeID, node)


updateNode :: IORef Batch -> UpdateNode.Args -> Script UpdateNode.Result
updateNode batchHandler (UpdateNode.Args tnode tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateNode"
    bc <- tryRight $ decode tbc
    (nodeID, node) <- tryRight $ decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchG.updateNode (nodeID, node) bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateNode.Result


removeNode :: IORef Batch -> RemoveNode.Args -> Script RemoveNode.Result
removeNode batchHandler (RemoveNode.Args tnodeID tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called removeNode"
    bc <- tryRight $ decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchG.removeNode nodeID bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return RemoveNode.Result


connect :: IORef Batch -> Connect.Args -> Script Connect.Result
connect batchHandler (Connect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called connect"
    bc <- tryRight $ decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = fmap decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return Connect.Result


disconnect :: IORef Batch -> Disconnect.Args -> Script Disconnect.Result
disconnect batchHandler (Disconnect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called disconnect"
    bc <- tryRight $ decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = fmap decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return Disconnect.Result
