---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Flowbox.ProjectManager.RPC.Handler.Graph where

import           Control.Monad
import qualified Data.Either as Either

import qualified Flowbox.Batch.Handler.Common                                                                 as Batch
import qualified Flowbox.Batch.Handler.Graph                                                                  as BatchG
import           Flowbox.Bus.Data.Message                                                                     as Message
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message                                          ()
import           Flowbox.Bus.RPC.RPC                                                                          (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                              hiding (Context, error)
import           Flowbox.ProjectManager.Context                                                               (Context)
import           Flowbox.System.Log.Logger
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


nodeAdd :: NodeAdd.Request -> RPC Context IO NodeAdd.Update
nodeAdd request@(NodeAdd.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    (_ :: Int, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.addNode node bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeAdd.Update request (encode (newNodeID, node)) updateNo

--nodeAdd2 :: NodeAdd.Request -> RPC Context IO (NodeAdd.Update, Register.Request)
--nodeAdd2 request@(NodeAdd.Request tnode tbc tlibID tprojectID astID) = do
--    bc <- decodeE tbc
--    (_ :: Int, node) <- decodeE tnode
--    let libID     = decodeP tlibID
--        projectID = decodeP tprojectID
--    newNodeID <- BatchG.addNode node bc libID projectID
--    updateNo <- Batch.getUpdateNo
--    return $ ( NodeAdd.Update request (encode (newNodeID, node)) updateNo
--             , Register.Request
--                (encodeP $ Message.mk "project.library.ast.function.graph.node.rm.request" $ NodeRemove.Request (encode newNodeID) tbc tlibID tprojectID astID)
--                (encodeP $ Message.mk "project.library.ast.function.graph.node.add.request" $ request)
--             )

nodeModify :: NodeModify.Request -> RPC Context IO NodeModify.Update
nodeModify request@(NodeModify.Request tnode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    (nodeID, node) <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    newNodeID <- BatchG.updateNode (nodeID, node) bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeModify.Update request (encode (newNodeID, node)) updateNo

nodeMIP :: NodeModifyInPlace.Request -> RPC Context IO (NodeModifyInPlace.Update)
nodeMIP = liftM fst . nodeModifyInPlace

nodeModifyInPlace :: NodeModifyInPlace.Request -> RPC Context IO (NodeModifyInPlace.Update, Register.Request)
nodeModifyInPlace request@(NodeModifyInPlace.Request tnode tbc tlibID tprojectID astID) = do
    bc <- decodeE tbc
    nodeWithId <- decodeE tnode
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    
    node <- BatchG.nodeByID (fst nodeWithId) bc libID projectID
    let oldNode   = encode (fst nodeWithId, node)

    BatchG.updateNodeInPlace nodeWithId bc libID projectID
    updateNo <- Batch.getUpdateNo

    logger error $ show oldNode ++ " " ++ (show tnode)
    return $ ( NodeModifyInPlace.Update request updateNo
             , Register.Request 
                (encodeP $ Message.mk "project.library.ast.function.graph.node.mip.request" $ NodeModifyInPlace.Request oldNode tbc tlibID tprojectID astID)
                (encodeP $ Message.mk "project.library.ast.function.graph.node.mip.request" $ request)
             )

--nodeRm :: NodeRemove.Request -> RPC Context IO NodeRemove.Update

nodeRemove :: NodeRemove.Request -> RPC Context IO NodeRemove.Update
nodeRemove request@(NodeRemove.Request tnodeIDs tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeIDs   = decodeP tnodeIDs
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.removeNodes nodeIDs bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeRemove.Update request updateNo


connect :: Connect.Request -> RPC Context IO Connect.Update
connect request@(Connect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.connect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Connect.Update request updateNo


disconnect :: Disconnect.Request -> RPC Context IO Disconnect.Update
disconnect request@(Disconnect.Request tsrcNodeID tsrcPort tdstNodeID tdstPort tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let srcNodeID = decodeP tsrcNodeID
        srcPort   = decodeP tsrcPort
        dstNodeID = decodeP tdstNodeID
        dstPort   = decodeP tdstPort
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchG.disconnect srcNodeID srcPort dstNodeID dstPort bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Disconnect.Update request updateNo
