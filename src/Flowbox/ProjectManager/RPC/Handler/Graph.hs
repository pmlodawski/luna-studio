---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.ProjectManager.RPC.Handler.Graph where

import qualified Flowbox.Batch.Handler.Common                                                                 as Batch
import qualified Flowbox.Batch.Handler.Graph                                                                  as BatchG
import           Flowbox.Bus.RPC.RPC                                                                          (RPC)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                                          ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView                                      ()
import           Flowbox.Prelude                                                                              hiding (Context)
import           Flowbox.ProjectManager.Context                                                               (Context)
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
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Graph"


get :: GetGraph.Request -> RPC Context IO GetGraph.Status
get request@(GetGraph.Request tbc tlibID tprojectID _)  = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID

    graph <- BatchG.nodesGraph bc libID projectID
    return $ GetGraph.Status request (encode graph)


lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup request@(Lookup.Request tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    node  <- BatchG.nodeByID nodeID bc libID projectID
    return $ Lookup.Status request (encode (nodeID, node))


nodeAdd :: NodeAdd.Request -> RPC Context IO NodeAdd.Update
nodeAdd request@(NodeAdd.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    (_ :: Int, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.addNode node bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeAdd.Update request (encode (newNodeID, node)) updateNo


nodeModify :: NodeModify.Request -> RPC Context IO NodeModify.Update
nodeModify request@(NodeModify.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    (nodeID, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.updateNode (nodeID, node) bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeModify.Update request (encode (newNodeID, node)) updateNo


nodeModifyInPlace :: NodeModifyInPlace.Request -> RPC Context IO NodeModifyInPlace.Update
nodeModifyInPlace request@(NodeModifyInPlace.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    nodeWithId <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.updateNodeInPlace nodeWithId bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeModifyInPlace.Update request updateNo


nodeRemove :: NodeRemove.Request -> RPC Context IO NodeRemove.Update
nodeRemove request@(NodeRemove.Request tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.removeNode nodeID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeRemove.Update request updateNo


connect :: Connect.Request -> RPC Context IO Connect.Update
connect request@(Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Connect.Update request updateNo


disconnect :: Disconnect.Request -> RPC Context IO Disconnect.Update
disconnect request@(Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeListP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeListP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Disconnect.Update request updateNo
