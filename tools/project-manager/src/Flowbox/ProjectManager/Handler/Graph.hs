---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.ProjectManager.Handler.Graph where

import qualified Data.IORef                                                                                  as IORef
import qualified Flowbox.Batch.Handler.Graph                                                                 as BatchG
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView                                     ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                              (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Args              as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Result            as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Args           as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Result         as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Args                  as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Result                as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Args               as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Result             as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Args             as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Result           as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Args          as NodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Result        as NodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Update.Args          as NodeUpdate
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Update.Result        as NodeUpdate
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.UpdateInPlace.Args   as NodeUpdateInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.UpdateInPlace.Result as NodeUpdateInPlace



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handlers.Graph"


getGraph :: ContextRef -> GetGraph.Args -> IO GetGraph.Result
getGraph ctxRef (GetGraph.Args tbc tlibID tprojectID) = do
    bc <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    ng <- BatchG.nodesGraph bc libID projectID batch
    return $ GetGraph.Result $ encode ng


lookup :: ContextRef -> Lookup.Args -> IO Lookup.Result
lookup ctxRef (Lookup.Args tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    node  <- BatchG.nodeByID nodeID bc libID projectID batch
    return $ Lookup.Result $ encode (nodeID, node)


nodeAdd :: ContextRef -> NodeAdd.Args -> IO NodeAdd.Result
nodeAdd ctxRef (NodeAdd.Args tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    (_ :: Int, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, newNodeID) <- BatchG.addNode node bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeAdd.Result $ encodeP newNodeID


nodeUpdate :: ContextRef -> NodeUpdate.Args -> IO NodeUpdate.Result
nodeUpdate ctxRef (NodeUpdate.Args tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    nodeWithId <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, newNodeID) <- BatchG.updateNode nodeWithId bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeUpdate.Result $ encodeP newNodeID


nodeUpdateInPlace :: ContextRef -> NodeUpdateInPlace.Args -> IO NodeUpdateInPlace.Result
nodeUpdateInPlace ctxRef (NodeUpdateInPlace.Args tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    nodeWithId <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.updateNodeInPlace nodeWithId bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return NodeUpdateInPlace.Result


nodeRemove :: ContextRef -> NodeRemove.Args -> IO NodeRemove.Result
nodeRemove ctxRef (NodeRemove.Args tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.removeNode nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return NodeRemove.Result


connect :: ContextRef -> Connect.Args -> IO Connect.Result
connect ctxRef (Connect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return Connect.Result


disconnect :: ContextRef -> Disconnect.Args -> IO Disconnect.Result
disconnect ctxRef (Disconnect.Args tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return Disconnect.Result
