---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.Properties where

import qualified Data.Bimap as Bimap
import           Data.Maybe (isJust)

import qualified Flowbox.Batch.Batch                                                                           as Batch
import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.Properties                                                              as BatchP
import           Flowbox.Bus.Data.Message                                                                      (Message)
import           Flowbox.Bus.Data.Topic                                                                        (Topic)
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                               hiding (Context)
import           Flowbox.ProjectManager.Context                                                                (Context)
import           Flowbox.ProjectManager.RPC.Handler.Graph                                                      (mapID)
import qualified Flowbox.ProjectManager.RPC.Topic                                                              as Topic
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.RPC.Handler.Handler                                                        (prepareResponse)
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Get.Request as GetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Get.Status  as GetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Request as SetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Update  as SetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Request                     as GetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Status                      as GetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Request                     as SetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Update                      as SetASTProperties
import           Luna.DEP.Data.Serialize.Proto.Conversion.Attributes                                           ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Library                                              ()



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


getASTProperties :: GetASTProperties.Request -> RPC Context IO GetASTProperties.Status
getASTProperties request@(GetASTProperties.Request tnodeID tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- BatchP.getProperties nodeID libID projectID
    return $ GetASTProperties.Status request $ encode properties


setASTProperties :: SetASTProperties.Request -> Maybe Topic -> RPC Context IO ([SetASTProperties.Update], [Message])
setASTProperties request@(SetASTProperties.Request tproperties tnodeID tlibID tprojectID) undoTopic = do
    properties <- decodeE tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    toldProperties <- encode <$> BatchP.getProperties nodeID libID projectID
    BatchP.setProperties properties nodeID libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstPropertiesSetRequest
                    (SetASTProperties.Request toldProperties tnodeID tlibID tprojectID)
                    Topic.projectLibraryAstPropertiesSetRequest
                    request
                    undoTopic
                    "set AST properties"
                    =<< SetASTProperties.Update request <$> Batch.getUpdateNo


getNodeProperties :: GetNodeProperties.Request -> RPC Context IO GetNodeProperties.Status
getNodeProperties request@(GetNodeProperties.Request tnodeID _ tlibID tprojectID _) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- BatchP.getProperties nodeID libID projectID
    return $ GetNodeProperties.Status request (encode properties)


setNodeProperties :: SetNodeProperties.Request -> Maybe Topic -> RPC Context IO ([SetNodeProperties.Update], [Message])
setNodeProperties (SetNodeProperties.Request tproperties tnodeID tbc tlibID tprojectID astID) undoTopic = do
    properties <- decodeE tproperties
    context <- Batch.get
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
        newID     = if isJust undoTopic then nodeID else mapID context Bimap.lookupR nodeID
    BatchP.setProperties properties newID libID projectID
    updateNo <- Batch.getUpdateNo
    return ([SetNodeProperties.Update (SetNodeProperties.Request tproperties (encodeP newID) tbc tlibID tprojectID astID) updateNo], [])
