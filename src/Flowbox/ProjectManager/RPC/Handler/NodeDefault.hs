---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.NodeDefault where

import qualified Data.Bimap as Bimap
import           Data.Maybe (isJust)

import qualified Flowbox.Batch.Batch                                                                           as Batch
import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.NodeDefault                                                             as BatchND
import           Flowbox.Bus.Data.Message                                                                      (Message)
import           Flowbox.Bus.Data.Topic                                                                        (Topic)
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                               hiding (Context)
import           Flowbox.ProjectManager.Context                                                                (Context)
import           Flowbox.ProjectManager.RPC.Handler.Graph                                                      (mapID)
import qualified Flowbox.ProjectManager.RPC.Topic                                                              as Topic
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Utils                                                             (makeMsgArr, prepareResponse, serialize)
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Request    as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Status     as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Request as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Update  as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Request    as NodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Update     as NodeDefaultSet
import qualified Generated.Proto.Urm.URM.Register.Request                                                      as Register
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb                                                ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Graph                                                ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.NodeDefault                                          ()
import qualified Luna.DEP.Graph.View.Default.DefaultsMap                                                       as DefaultsMap
import qualified Luna.DEP.Graph.View.Default.Expr                                                              as DefaultExpr



logger :: LoggerIO
logger = getLoggerIO $moduleName


get :: NodeDefaultGet.Request -> RPC Context IO NodeDefaultGet.Status
get request@(NodeDefaultGet.Request tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    nodeDefaults <- BatchND.nodeDefaults nodeID bc libID projectID
    return $ NodeDefaultGet.Status request (encode nodeDefaults)


set :: NodeDefaultSet.Request -> Maybe Topic -> RPC Context IO ([NodeDefaultSet.Update], [Message])
set (NodeDefaultSet.Request tdstPort tvalue tnodeID tbc tlibID tprojectID astID) undoTopic = do
    bc      <- decodeE tbc
    value   <- decodeE tvalue
    context <- Batch.get
    let dstPort   = decodeP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
        originID  = if isJust undoTopic then mapID context Bimap.lookup nodeID else nodeID
        newID     = if isJust undoTopic then nodeID else mapID context Bimap.lookupR nodeID
        newRequest nid val = NodeDefaultSet.Request tdstPort (encode val) (encodeP nid) tbc tlibID tprojectID astID
    defaultsMap <- BatchND.nodeDefaults newID bc libID projectID
    BatchND.setNodeDefault dstPort value newID bc libID projectID
    updateNo  <- Batch.getUpdateNo
    return ( [NodeDefaultSet.Update (newRequest newID value) updateNo]
           , maybe []
                   (\bmp -> makeMsgArr (Register.Request
                                            (serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest) $ newRequest originID $ view DefaultExpr.nodeExpr bmp)
                                            (serialize ("undone." <> Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest) $ newRequest originID value)
                                            tprojectID
                                            (encodeP $ "set port " <> show dstPort ++ " in " ++ show nodeID)
                                       ) undoTopic)
                   $ DefaultsMap.lookup dstPort defaultsMap
           )
--    prepareResponse projectID
--                    Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest
--                    (maybe (NodeDefaultRemove.Request tdstPort (encodeP originID) tbc tlibID tprojectID astID)
--                           (newRequest originID . snd)
--                           $ DefaultsMap.lookup dstPort defaultsMap
--                    )
--                    Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest
--                    (newRequest originID value)
--                    undoTopic
--                    =<< NodeDefaultSet.Update (newRequest newID value) <$> Batch.getUpdateNo


remove :: NodeDefaultRemove.Request -> RPC Context IO NodeDefaultRemove.Update
remove request@(NodeDefaultRemove.Request tdstPort tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let dstPort   = decodeP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchND.removeNodeDefault dstPort nodeID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeDefaultRemove.Update request updateNo
