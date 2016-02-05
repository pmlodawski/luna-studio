---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Flowbox.ProjectManager.RPC.Handler.Graph where

import qualified Data.Bimap                                                                                    as Bimap
import qualified Data.Either                                                                                   as Either
import           Data.Maybe                                                                                    (fromMaybe, isJust)
import           Data.Sequence                                                                                 ((><))
import qualified Data.Sequence                                                                                 as Sequence
import qualified Data.Set                                                                                      as Set

import qualified Flowbox.Batch.Batch                                                                           as Batch
import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.Graph                                                                   as BatchG
import qualified Flowbox.Batch.Handler.NodeDefault                                                             as BatchND
import qualified Flowbox.Batch.Handler.Properties                                                              as BatchP
import           Flowbox.Bus.Data.Message                                                                      (Message)
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message                                           ()
import           Flowbox.Bus.Data.Topic                                                                        (Topic)
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                               hiding (Context, error)
import           Flowbox.ProjectManager.Context                                                                (Context)
import qualified Flowbox.ProjectManager.RPC.Topic                                                              as Topic
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Utils                                                                      (makeMsgArr, prepareResponse,
                                                                                                                serialize)
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request             as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Update              as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Request          as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Update           as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Request                 as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Status                  as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Request              as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Lookup.Status               as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.LookupMany.Request          as LookupMany
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.LookupMany.Status           as LookupMany
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request            as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update             as NodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Request    as NodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Request         as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Update          as NodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Request  as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Update   as NodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Request as SetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Request         as NodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Update          as NodeRemove
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request                                              as RegisterMultiple
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb                                                ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.GraphView                                            ()
import qualified Luna.DEP.Graph.Node                                                                           as Node
import qualified Luna.DEP.Graph.Node.Expr                                                                      as NodeExpr
import qualified Luna.DEP.Graph.Node.StringExpr                                                                as StringExpr
import qualified Luna.DEP.Graph.View.Default.DefaultsMap                                                       as DefaultsMap
import qualified Luna.DEP.Graph.View.Default.Expr                                                              as DefaultExpr
import           Luna.DEP.Graph.View.EdgeView                                                                  (EdgeView (EdgeView))



nodeName :: Node.Node -> String
nodeName = fromMaybe "" . maybe Nothing (^? StringExpr.string) . maybe Nothing (^? NodeExpr.strExpr) . (^? Node.expr)
--nodeName = fromMaybe "" . (^? StringExpr.string) =<< (^? NodeExpr.strExpr) =<< (^? Node.expr)
--nodeName = (^?! StringExpr.string) . (^?! NodeExpr.strExpr) . (^?! Node.expr)


logger :: LoggerIO
logger = getLoggerIO $moduleName


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
    (oldNodeID, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.addNode node bc libID projectID
    context <- Batch.get
    Batch.put $ Batch.idMap %~ Bimap.insert newNodeID ( case oldNodeID of
                                                          -1  -> newNodeID
                                                          nid -> nid
                                                      ) $ context
    --logger error $ "oldNodeID " ++ show oldNodeID
    --logger error $ "newNodeID " ++ show newNodeID
    -- ZWIEKSZYC CZYTELNOSC nizej
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionGraphNodeRemoveRequest
                    (NodeRemove.Request (Sequence.singleton $ encodeP newNodeID) tbc tlibID tprojectID astID)
                    Topic.projectLibraryAstFunctionGraphNodeAddRequest
                    (NodeAdd.Request (encode (mapID context Bimap.lookup newNodeID, node)) tbc tlibID tprojectID astID)
                    undoTopic
                    ("add node " ++ nodeName node)
                    =<< NodeAdd.Update request (encode (newNodeID, node)) <$> Batch.getUpdateNo

nodeModify :: NodeModify.Request -> Maybe Topic -> RPC Context IO ([NodeModify.Update], [Message])
nodeModify (NodeModify.Request tnode tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    context <- Batch.get
    (nodeID, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        originID  = if isJust undoTopic then mapID context Bimap.lookup nodeID else nodeID
        newID     = if isJust undoTopic then nodeID else mapID context Bimap.lookupR nodeID
    oldNode <- BatchG.nodeByID newID bc libID projectID
    newNodeID <- BatchG.updateNode (newID, node) bc libID projectID
    let toldNode = encode (originID, oldNode)
        tnewNode = encode (originID, node)
    context2 <- Batch.get
    Batch.put $ (Batch.idMap %~ Bimap.insert newNodeID originID) context2
    --logger error $ "nodeID " ++ show nodeID
    --logger error $ "newNodeID" ++ show newNodeID
    --logger error $ "newID "  ++ show newID
    --logger error $ "originID" ++ show originID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionGraphNodeModifyRequest
                    (NodeModify.Request toldNode tbc tlibID tprojectID astID)
                    Topic.projectLibraryAstFunctionGraphNodeModifyRequest
                    (NodeModify.Request tnewNode tbc tlibID tprojectID astID)
                    undoTopic
                    ("modify node " ++ nodeName node)
                    =<< NodeModify.Update (NodeModify.Request (encode (newID, node)) tbc tlibID tprojectID astID) (encode (newNodeID, node)) <$> Batch.getUpdateNo


nodeModifyInPlace :: NodeModifyInPlace.Request -> Maybe Topic -> RPC Context IO ([NodeModifyInPlace.Update], [Message])
nodeModifyInPlace (NodeModifyInPlace.Request tnode tbc tlibID tprojectID astID) undoTopic = do
    bc             <- decodeE tbc
    (nid, newNode) <- decodeE tnode
    context        <- Batch.get
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        originID  = if isJust undoTopic then mapID context Bimap.lookup nid else nid
        newID     = if isJust undoTopic then nid else mapID context Bimap.lookupR nid
        newRequest nodeId = NodeModifyInPlace.Request (encode (nodeId, newNode)) tbc tlibID tprojectID astID

    oldNd <- BatchG.nodeByID newID bc libID projectID
    let oldNode   = encode (originID, oldNd)
    BatchG.updateNodeInPlace (newID, newNode) bc libID projectID
    --logger error $ "nid " ++ show nid
    --logger error $ "newID "  ++ show newID
    --logger error $ "originID" ++ show originID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest
                    (NodeModifyInPlace.Request oldNode tbc tlibID tprojectID astID)
                    Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest
                    (newRequest originID)
                    undoTopic
                    ("modify node " ++ nodeName newNode)
                    =<< NodeModifyInPlace.Update (newRequest newID) <$> Batch.getUpdateNo


nodeRemove :: NodeRemove.Request -> Maybe Topic -> RPC Context IO ([NodeRemove.Update], [Message])
nodeRemove (NodeRemove.Request tnodeIDs tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    context <- Batch.get
    let nodeIDs            = decodeP tnodeIDs
        mapIDs bimapLookup = map (mapID context bimapLookup) nodeIDs
        updatedRequest ids = NodeRemove.Request ids tbc tlibID tprojectID astID
        originID nid       = if isJust undoTopic then mapID context Bimap.lookup nid else nid
        originIDs          = if isJust undoTopic then mapIDs Bimap.lookup else nodeIDs
        newIDs             = if isJust undoTopic then nodeIDs else mapIDs Bimap.lookupR
        libID              = decodeP tlibID
        projectID          = decodeP tprojectID

    oldNodes <- mapM (\nid -> BatchG.nodeByID (mapID context Bimap.lookupR nid) bc libID projectID) originIDs
    let toldNodes = zipWith (\nid node -> encode (nid, node)) originIDs oldNodes

    -- KOMENTARZ MARKA: co to robi? Zrobmy tak by kod byl jasny
    -- lambdy wydziel do whera jako funkcje i wiecej zmiennych!
    rm <- mapM (\nid -> BatchG.nodeEdges nid bc libID projectID) newIDs
    let removed = Set.toList $ Set.fromList $ concat rm
    defaults <- mapM (\nid -> do defaults <- BatchND.nodeDefaults nid bc libID projectID
                                 return $ DefaultsMap.mapWithKey (\k v -> serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest)
                                                                              $ NodeDefaultSet.Request (encodeP k) (encode $ view DefaultExpr.nodeExpr v) (encodeP $ originID nid) tbc tlibID tprojectID astID
                                                                 )
                                                                 defaults
                     )
                     newIDs
    properties <- mapM (\nid -> do properties <- BatchP.getProperties nid libID projectID
                                   return $ serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest)
                                                $ SetNodeProperties.Request (encode properties) (encodeP $ originID nid) tbc tlibID tprojectID astID
                       )
                       newIDs

    BatchG.removeNodes newIDs bc libID projectID
    updateNo <- Batch.getUpdateNo

    --logger error $ "newIDs "  ++ show newIDs
    --logger error $ "originIDs" ++ show originIDs
    --logger warning $ show $ map snd . DefaultsMap.toList =<< defaults
    --logger warning $ show $ map snd $ DefaultsMap.toList =<< defaults
    return ( [NodeRemove.Update (updatedRequest $ encodeP newIDs) updateNo]
           , makeMsgArr (RegisterMultiple.Request
                            (  (Sequence.fromList $ map (\node -> serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodeAddRequest) $ NodeAdd.Request node tbc tlibID tprojectID astID) $ toldNodes)
                            >< (Sequence.fromList $ map (\(srcID, dstID, EdgeView srcPorts dstPorts) -> serialize ("undone." <> Topic.projectLibraryAstFunctionGraphConnectRequest)
                                                                                                            $ Connect.Request (encodeP $ originID srcID)
                                                                                                                              (encodeP srcPorts)
                                                                                                                              (encodeP $ originID dstID)
                                                                                                                              (encodeP dstPorts)
                                                                                                                              tbc
                                                                                                                              tlibID
                                                                                                                              tprojectID
                                                                                                                              astID
                                                        )
                                                        removed
                               )
                            >< (Sequence.fromList $ map snd . DefaultsMap.toList =<< defaults)
                            >< (Sequence.fromList properties)
                            )
                            (serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodeRemoveRequest) $ updatedRequest $ encodeP originIDs)
                            tprojectID
                            (encodeP $ "remove nodes " ++ (show $ map (nodeName) oldNodes))
                        ) undoTopic
           )


connect :: Connect.Request -> Maybe Topic -> RPC Context IO ([Connect.Update], [Message])
connect (Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) undoTopic = do
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
        newRequest constr sid did = constr (encodeP sid) tsrcPort (encodeP did) tdstPort tbc tlibID tprojectID astID
    BatchG.connect newSID srcPort newDID dstPort bc libID projectID
    --logger error $ "srcNodeID " ++ show srcNodeID
    --logger error $ "srcPort " ++ show srcPort
    --logger error $ "newSID "  ++ show newSID
    --logger error $ "originSID" ++ show originSID
    --logger error $ "dstNodeID " ++ show dstNodeID
    --logger error $ "dstPort " ++ show dstPort
    --logger error $ "newDID "  ++ show newDID
    --logger error $ "originDID" ++ show originDID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionGraphDisconnectRequest
                    (newRequest Disconnect.Request originSID originDID)
                    Topic.projectLibraryAstFunctionGraphConnectRequest
                    (newRequest Connect.Request originSID originDID)
                    undoTopic
                    ("connect " ++ (show srcNodeID) ++ " with " ++ (show dstNodeID))
                    =<< Connect.Update (newRequest Connect.Request newSID newDID) <$> Batch.getUpdateNo

disconnect :: Disconnect.Request -> Maybe Topic -> RPC Context IO ([Disconnect.Update], [Message])
disconnect (Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID astID) undoTopic = do
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
        newRequest constr sid did = constr (encodeP sid) tsrcPort (encodeP did) tdstPort tbc tlibID tprojectID astID
    BatchG.disconnect newSID srcPort newDID dstPort bc libID projectID
    --logger error $ "srcNodeID " ++ show srcNodeID
    --logger error $ "srcPort " ++ show srcPort
    --logger error $ "newSID "  ++ show newSID
    --logger error $ "originSID" ++ show originSID
    --logger error $ "dstNodeID " ++ show dstNodeID
    --logger error $ "dstPort " ++ show dstPort
    --logger error $ "newDID "  ++ show newDID
    --logger error $ "originDID" ++ show originDID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionGraphConnectRequest
                    (newRequest Connect.Request originSID originDID)
                    Topic.projectLibraryAstFunctionGraphDisconnectRequest
                    (newRequest Disconnect.Request originSID originDID)
                    undoTopic
                    ("disconnect " ++ (show srcNodeID) ++ " from the " ++ (show dstNodeID))
                    =<< Disconnect.Update (newRequest Disconnect.Request newSID newDID) <$> Batch.getUpdateNo


mapID :: Batch.BatchEnv -> (a -> Batch.IDMap -> Maybe a) -> a -> a
mapID context bimaplookup nid = fromMaybe nid $ bimaplookup nid $ context ^. Batch.idMap
