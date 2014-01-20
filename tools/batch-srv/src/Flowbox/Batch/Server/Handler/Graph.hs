---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.Batch.Server.Handler.Graph where

import Data.IORef (IORef)

import qualified Data.IORef                                              as IORef
import           Flowbox.Batch.Batch                                     (Batch)
import qualified Flowbox.Batch.Handler.Graph                             as BatchG
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Graph.AddNode.Args                as AddNode
import qualified Generated.Proto.Batch.Graph.AddNode.Result              as AddNode
import qualified Generated.Proto.Batch.Graph.Connect.Args                as Connect
import qualified Generated.Proto.Batch.Graph.Connect.Result              as Connect
import qualified Generated.Proto.Batch.Graph.Disconnect.Args             as Disconnect
import qualified Generated.Proto.Batch.Graph.Disconnect.Result           as Disconnect
import qualified Generated.Proto.Batch.Graph.NodeByID.Args               as NodeByID
import qualified Generated.Proto.Batch.Graph.NodeByID.Result             as NodeByID
import qualified Generated.Proto.Batch.Graph.NodesGraph.Args             as NodesGraph
import qualified Generated.Proto.Batch.Graph.NodesGraph.Result           as NodesGraph
import qualified Generated.Proto.Batch.Graph.RemoveNode.Args             as RemoveNode
import qualified Generated.Proto.Batch.Graph.RemoveNode.Result           as RemoveNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Args             as UpdateNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Result           as UpdateNode


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Graph"


nodesGraph :: IORef Batch -> NodesGraph.Args -> IO NodesGraph.Result
nodesGraph batchHandler (NodesGraph.Args tbc tlibID tprojectID) = do
    loggerIO info "called nodesGraph"
    bc <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    ng <- BatchG.nodesGraph bc libID projectID batch
    return $ NodesGraph.Result $ encode ng


nodeByID :: IORef Batch -> NodeByID.Args -> IO NodeByID.Result
nodeByID batchHandler (NodeByID.Args tnodeID tbc tlibID tprojectID) = do
    loggerIO info "called nodeByID"
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    node  <- BatchG.nodeByID nodeID bc libID projectID batch
    return $ NodeByID.Result $ encode (nodeID, node)


addNode :: IORef Batch -> AddNode.Args -> IO AddNode.Result
addNode batchHandler (AddNode.Args tnode tbc tlibID tprojectID) = do
    loggerIO info "called addNode"
    bc <- decode tbc
    (_ :: Int, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    (newBatch, newNodeID) <- BatchG.addNode node bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return $ AddNode.Result $ encodeP newNodeID


updateNode :: IORef Batch -> UpdateNode.Args -> IO UpdateNode.Result
updateNode batchHandler (UpdateNode.Args tnode tbc tlibID tprojectID) = do
    loggerIO info "called addNode"
    bc <- decode tbc
    nodeWithId <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    (newBatch, newNodeID) <- BatchG.updateNode nodeWithId bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return $ UpdateNode.Result $ encodeP newNodeID


removeNode :: IORef Batch -> RemoveNode.Args -> IO RemoveNode.Result
removeNode batchHandler (RemoveNode.Args tnodeID tbc tlibID tprojectID) = do
    loggerIO info "called removeNode"
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchG.removeNode nodeID bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return RemoveNode.Result


connect :: IORef Batch -> Connect.Args -> IO Connect.Result
connect batchHandler (Connect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    loggerIO info "called connect"
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return Connect.Result


disconnect :: IORef Batch -> Disconnect.Args -> IO Disconnect.Result
disconnect batchHandler (Disconnect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    loggerIO info "called disconnect"
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return Disconnect.Result
