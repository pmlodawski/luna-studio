---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Flowbox.ProjectManager.RPC.Handler.Graph where

import qualified Data.Either   as Either
import qualified Data.List     as List
import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Handler.Common                                                                 as Batch
import qualified Flowbox.Batch.Handler.Graph                                                                  as BatchG
import           Flowbox.Bus.Data.Message                                                                     as Message
import           Flowbox.Bus.Data.Topic                                                                       (Topic)
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message                                          ()
import           Flowbox.Bus.RPC.RPC                                                                          (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                              hiding (Context, error)
import           Flowbox.ProjectManager.Context                                                               (Context)
import qualified Flowbox.ProjectManager.RPC.Topic                                                             as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                                                                 as Proto
import qualified Generated.Proto.Bus.Message                                                                  as Bus
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request            as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Update             as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Request         as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Update          as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Request                as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Status                 as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Request             as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Status              as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.LookupMany.Request         as LookupMany
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.LookupMany.Status          as LookupMany
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request           as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update            as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Request        as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Update         as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Request as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Update  as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Request        as NodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Update         as NodeRemove
import qualified Generated.Proto.Urm.URM.Register.Request                                                     as Register
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request                                             as RegisterMultiple
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb                                               ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.GraphView                                           ()



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


get :: GetGraph.Request -> RPC Context IO GetGraph.Status
get request@(GetGraph.Request tbc tlibID tprojectID _)  = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID

    graph <- BatchG.nodesGraph bc libID projectID
    seq graph $ return ()
    return $ GetGraph.Status request (encode graph)


lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup request@(Lookup.Request tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    node  <- BatchG.nodeByID nodeID bc libID projectID
    return $ Lookup.Status request (encode (nodeID, node))


lookupMany :: LookupMany.Request -> RPC Context IO LookupMany.Status
lookupMany request@(LookupMany.Request tnodeIDs tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeIDs   = decodeP tnodeIDs
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    nodes  <- BatchG.nodesByIDs nodeIDs bc libID projectID
    let select (nodeID, Just node) = Right (nodeID, node)
        select (nodeID, Nothing  ) = Left nodeID
        items    = map select nodes
        found    = Either.rights items
        notFound = Either.lefts items
    return $ LookupMany.Status request (encode found) (encodeP notFound)

nodeAdd :: NodeAdd.Request -> RPC Context IO (NodeAdd.Update, Register.Request)
nodeAdd request@(NodeAdd.Request tnode tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    (oldNodeId :: Int, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.addNode node bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( NodeAdd.Update request (encode (newNodeID, node)) updateNo
             , Register.Request
                (fun Topic.projectLibraryAstFunctionGraphNodeRemoveRequest $ NodeRemove.Request (Sequence.singleton $ encodeP newNodeID) tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphNodeAddRequest request)
             )

nodeModify :: NodeModify.Request -> RPC Context IO NodeModify.Update
nodeModify request@(NodeModify.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    (nodeID, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.updateNode (nodeID, node) bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeModify.Update request (encode (newNodeID, node)) updateNo

nodeModifyInPlace :: NodeModifyInPlace.Request -> RPC Context IO (NodeModifyInPlace.Update, Register.Request)
nodeModifyInPlace request@(NodeModifyInPlace.Request tnode tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    nodeWithId@(nid, _) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    
    node <- BatchG.nodeByID nid bc libID projectID
    let oldNode   = encode (nid, node)

    BatchG.updateNodeInPlace nodeWithId bc libID projectID
    updateNo <- Batch.getUpdateNo

    logger error $ show oldNode ++ " " ++ (show tnode)
    return $ ( NodeModifyInPlace.Update request updateNo
             , Register.Request 
                (fun Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest $ NodeModifyInPlace.Request oldNode tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest request)
             )


nodeRemove :: NodeRemove.Request -> RPC Context IO (NodeRemove.Update, RegisterMultiple.Request)
nodeRemove request@(NodeRemove.Request tnodeIDs tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    let nodeIDs   = decodeP tnodeIDs
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    oldNodes <- mapM (\nid -> do node <- BatchG.nodeByID nid bc libID projectID
                                 return $ encode (nid, node))
                     $ List.sort nodeIDs
    BatchG.removeNodes nodeIDs bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( NodeRemove.Update request updateNo
             , RegisterMultiple.Request
                (Sequence.fromList $ map (\node -> fun Topic.projectLibraryAstFunctionGraphNodeAddRequest $ NodeAdd.Request node tbc tlibID tprojectID astID) oldNodes)
                (fun Topic.projectLibraryAstFunctionGraphNodeRemoveRequest request)
             )


connect :: Connect.Request -> RPC Context IO (Connect.Update, Register.Request)
connect request@(Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( Connect.Update request updateNo
             , Register.Request
                (fun Topic.projectLibraryAstFunctionGraphDisconnectRequest $ Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphConnectRequest request)
             )

fun :: Proto.Serializable message => Topic -> message -> Bus.Message
fun = (encodeP .) . Message.mk . ("undone." ++)

disconnect :: Disconnect.Request -> RPC Context IO (Disconnect.Update, Register.Request)
disconnect request@(Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( Disconnect.Update request updateNo
             , Register.Request
                (fun Topic.projectLibraryAstFunctionGraphConnectRequest $ Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphDisconnectRequest request)
             )
