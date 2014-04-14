---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.ProjectManager.Handler.Graph where

import qualified Data.IORef                                                                                   as IORef
import qualified Flowbox.Batch.Handler.Graph                                                                  as BatchG
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView                                      ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                               (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request            as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Update             as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Request         as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Update          as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Request                as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Status                 as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Request             as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Status              as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request           as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update            as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Request        as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Update         as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Request as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Update  as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Request        as NodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Update         as NodeRemove



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handlers.Graph"


get :: ContextRef -> GetGraph.Request -> IO GetGraph.Status
get ctxRef (GetGraph.Request tbc tlibID tprojectID) = do
    bc <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    graph <- BatchG.nodesGraph bc libID projectID batch
    return $ GetGraph.Status (encode graph) tbc tlibID tprojectID


lookup :: ContextRef -> Lookup.Request -> IO Lookup.Status
lookup ctxRef (Lookup.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    node  <- BatchG.nodeByID nodeID bc libID projectID batch
    return $ Lookup.Status (encode (nodeID, node)) tbc tlibID tprojectID


nodeAdd :: ContextRef -> NodeAdd.Request -> IO NodeAdd.Update
nodeAdd ctxRef (NodeAdd.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    (_ :: Int, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, newNodeID) <- BatchG.addNode node bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeAdd.Update (encode (newNodeID, node)) tbc tlibID tprojectID


nodeModify :: ContextRef -> NodeModify.Request -> IO NodeModify.Update
nodeModify ctxRef (NodeModify.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    (nodeID, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, newNodeID) <- BatchG.updateNode (nodeID, node) bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeModify.Update (encodeP nodeID) (encode (newNodeID, node)) tbc tlibID tprojectID


nodeModifyInPlace :: ContextRef -> NodeModifyInPlace.Request -> IO NodeModifyInPlace.Update
nodeModifyInPlace ctxRef (NodeModifyInPlace.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    nodeWithId <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.updateNodeInPlace nodeWithId bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeModifyInPlace.Update tnode tbc tlibID tprojectID


nodeRemove :: ContextRef -> NodeRemove.Request -> IO NodeRemove.Update
nodeRemove ctxRef (NodeRemove.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchG.removeNode nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeRemove.Update tnodeID tbc tlibID tprojectID


connect :: ContextRef -> Connect.Request -> IO Connect.Update
connect ctxRef (Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
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
    return $ Connect.Update tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID


disconnect :: ContextRef -> Disconnect.Request -> IO Disconnect.Update
disconnect ctxRef (Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
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
    return $ Disconnect.Update tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID
