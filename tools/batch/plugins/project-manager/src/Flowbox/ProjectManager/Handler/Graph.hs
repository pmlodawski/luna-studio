---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.ProjectManager.Handler.Graph where

import qualified Flowbox.Batch.Handler.Graph                                                                  as BatchG
import           Flowbox.Bus.RPC.RPC                                                                          (RPC)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                                          ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView                                      ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                               (ContextRef)
import qualified Flowbox.ProjectManager.Context                                                               as Context
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handlers.Graph"


get :: ContextRef -> GetGraph.Request -> RPC GetGraph.Status
get ctxRef (GetGraph.Request tbc tlibID tprojectID) = do
    bc <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID

    graph <- Context.run ctxRef $ BatchG.nodesGraph bc libID projectID
    return $ GetGraph.Status (encode graph) tbc tlibID tprojectID


lookup :: ContextRef -> Lookup.Request -> RPC Lookup.Status
lookup ctxRef (Lookup.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    node  <- Context.run ctxRef $ BatchG.nodeByID nodeID bc libID projectID
    return $ Lookup.Status (encode (nodeID, node)) tbc tlibID tprojectID


nodeAdd :: ContextRef -> NodeAdd.Request -> RPC NodeAdd.Update
nodeAdd ctxRef (NodeAdd.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    (_ :: Int, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- Context.run ctxRef $ BatchG.addNode node bc libID projectID
    return $ NodeAdd.Update (encode (newNodeID, node)) tbc tlibID tprojectID


nodeModify :: ContextRef -> NodeModify.Request -> RPC NodeModify.Update
nodeModify ctxRef (NodeModify.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    (nodeID, node) <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- Context.run ctxRef $ BatchG.updateNode (nodeID, node) bc libID projectID
    return $ NodeModify.Update (encodeP nodeID) (encode (newNodeID, node)) tbc tlibID tprojectID


nodeModifyInPlace :: ContextRef -> NodeModifyInPlace.Request -> RPC NodeModifyInPlace.Update
nodeModifyInPlace ctxRef (NodeModifyInPlace.Request tnode tbc tlibID tprojectID) = do
    bc <- decode tbc
    nodeWithId <- decode tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchG.updateNodeInPlace nodeWithId bc libID projectID
    return $ NodeModifyInPlace.Update tnode tbc tlibID tprojectID


nodeRemove :: ContextRef -> NodeRemove.Request -> RPC NodeRemove.Update
nodeRemove ctxRef (NodeRemove.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchG.removeNode nodeID bc libID projectID
    return $ NodeRemove.Update tnodeID tbc tlibID tprojectID


connect :: ContextRef -> Connect.Request -> RPC Connect.Update
connect ctxRef (Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    return $ Connect.Update tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID


disconnect :: ContextRef -> Disconnect.Request -> RPC Disconnect.Update
disconnect ctxRef (Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID) = do
    bc <- decode tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $  BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    return $ Disconnect.Update tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID
