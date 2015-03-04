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

import qualified Data.Bimap    as Bimap   
import qualified Data.Either   as Either
import qualified Data.List     as List
import           Data.Maybe    (fromMaybe, isJust)
--import qualified Data.Set      as Set
import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Batch                                                                          as Batch
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

nodeAdd :: NodeAdd.Request -> Maybe Topic -> RPC Context IO ([NodeAdd.Update], [Message])
nodeAdd request@(NodeAdd.Request tnode tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    (oldNodeId, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.addNode node bc libID projectID
    updateNo <- Batch.getUpdateNo

    context <- Batch.get
    Batch.put $ Batch.idMap %~ Bimap.insert newNodeID ( case oldNodeId of
                                                           -1 -> newNodeID
                                                           nid -> nid
                                                      ) $ context 

    return $ ( [NodeAdd.Update request (encode (newNodeID, node)) updateNo]
             , makeMsgArr (Register.Request
                (fun Topic.projectLibraryAstFunctionGraphNodeRemoveRequest $ NodeRemove.Request (Sequence.singleton $ encodeP newNodeID) tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphNodeAddRequest $ NodeAdd.Request (encode (mapID context Bimap.lookup newNodeID, node)) tbc tlibID tprojectID astID)
                          ) undoTopic
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

nodeModifyInPlace :: NodeModifyInPlace.Request -> Maybe Topic -> RPC Context IO ([NodeModifyInPlace.Update], [Message])
nodeModifyInPlace request@(NodeModifyInPlace.Request tnode tbc tlibID tprojectID astID) undoTopic = do
    bc      <- decodeE tbc
    context <- Batch.get
    nodeWithId@(nid, newNode) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        originID  = if isJust undoTopic then mapID context Bimap.lookup nid else nid
        newID     = if isJust undoTopic then nid else mapID context Bimap.lookupR nid
        newRequest nid = NodeModifyInPlace.Request (encode (nid, newNode)) tbc tlibID tprojectID astID
    
    oldNd <- BatchG.nodeByID newID bc libID projectID
    let oldNode   = encode (originID, oldNd)

    BatchG.updateNodeInPlace (newID, newNode) bc libID projectID
    updateNo <- Batch.getUpdateNo

    return $ ( [NodeModifyInPlace.Update (newRequest newID) updateNo]
             , makeMsgArr (Register.Request 
                (fun Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest $ NodeModifyInPlace.Request oldNode tbc tlibID tprojectID astID)
                (fun Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest $ newRequest originID)
                          ) undoTopic
             )


nodeRemove :: NodeRemove.Request -> Maybe Topic -> RPC Context IO ([NodeRemove.Update], [Message])
nodeRemove (NodeRemove.Request tnodeIDs tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    context <- Batch.get
    let nodeIDs            = decodeP tnodeIDs
        mapIDs bimapLookup = map (mapID context bimapLookup) nodeIDs
        updatedRequest ids = NodeRemove.Request ids tbc tlibID tprojectID astID
        originIDs          = if isJust undoTopic then mapIDs Bimap.lookup else nodeIDs
        newIDs             = if isJust undoTopic then nodeIDs else mapIDs Bimap.lookupR
        libID              = decodeP tlibID
        projectID          = decodeP tprojectID

    oldNodes <- mapM (\nid -> do node <- BatchG.nodeByID (mapID context Bimap.lookupR nid) bc libID projectID
                                 return $ encode (nid, node)
                     ) $ originIDs

    let removed = Set.toList . Set.fromList . concat . map (\node -> BatchG.nodeEdges node bc libID projectID) oldNodes
    logger warning $ show removed
    
    BatchG.removeNodes newIDs bc libID projectID
    updateNo <- Batch.getUpdateNo

    return ( [NodeRemove.Update (updatedRequest $ encodeP newIDs) updateNo]
           , makeMsgArr (RegisterMultiple.Request
              (  Sequence.fromList $ map (\node -> fun Topic.projectLibraryAstFunctionGraphNodeAddRequest $ NodeAdd.Request node tbc tlibID tprojectID astID) $ oldNodes)
--              >< Sequence.fromList $ map (\node -> fun Topic.projectLibraryAstFunctionGraphConnectRequest $ Connect.Request )
              (fun Topic.projectLibraryAstFunctionGraphNodeRemoveRequest $ updatedRequest $ encodeP originIDs)
                        ) undoTopic
           )


connect :: Connect.Request -> Maybe Topic -> RPC Context IO ([Connect.Update], [Message])
connect request@(Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    context <- Batch.get
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
        originSID = if isJust undoTopic then mapID context Bimap.lookup srcNodeID else srcNodeID
        newSID    = if isJust undoTopic then srcNodeID else mapID context Bimap.lookupR srcNodeID
        originDID = if isJust undoTopic then mapID context Bimap.lookup dstNodeID else dstNodeID
        newDID    = if isJust undoTopic then dstNodeID else mapID context Bimap.lookupR dstNodeID
        newRequest sid did = Connect.Request (encodeP sid) tsrcPort (encodeP did) tdstPort tbc tlibID tprojectID astID

    BatchG.connect newSID srcPort newDID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( [Connect.Update (newRequest newSID newDID) updateNo]
             , makeMsgArr (Register.Request (fun Topic.projectLibraryAstFunctionGraphDisconnectRequest $ Disconnect.Request (encodeP originSID) tsrcPort (encodeP originDID) tdstPort tbc tlibID tprojectID astID)
                                            (fun Topic.projectLibraryAstFunctionGraphConnectRequest $ newRequest originSID originDID)
                          ) undoTopic
             )

disconnect :: Disconnect.Request -> Maybe Topic -> RPC Context IO ([Disconnect.Update], [Message])
disconnect request@(Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ( [Disconnect.Update request updateNo]
             , makeMsgArr (Register.Request
                            (fun Topic.projectLibraryAstFunctionGraphConnectRequest $ Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID)
                            (fun Topic.projectLibraryAstFunctionGraphDisconnectRequest request)
                          ) undoTopic
             )


fun :: Proto.Serializable message => Topic -> message -> Bus.Message
fun = (encodeP .) . Message.mk . ("undone." ++)


mapID :: Batch.BatchEnv -> (a -> Batch.IDMap -> Maybe a) -> a -> a
mapID context bimaplookup nid = fromMaybe nid $ bimaplookup nid $ context ^. Batch.idMap


makeMsgArr :: (Proto.ReflectDescriptor request, Proto.Wire request) => request -> Maybe Topic -> [Message]
makeMsgArr request = maybe [] $ return . (flip Message.mk request)
