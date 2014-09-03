---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Properties where

import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.Properties                                                              as BatchP
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Prelude                                                                               hiding (Context)
import           Flowbox.ProjectManager.Context                                                                (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Get.Request as GetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Get.Status  as GetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Request as SetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Update  as SetNodeProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Request                     as GetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Status                      as GetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Request                     as SetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Update                      as SetASTProperties
import           Luna.Data.Serialize.Proto.Conversion.Attributes                                               ()



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Properties"


getASTProperties :: GetASTProperties.Request -> RPC Context IO GetASTProperties.Status
getASTProperties request@(GetASTProperties.Request tnodeID tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- BatchP.getProperties nodeID libID projectID
    return $ GetASTProperties.Status request $ encode properties


setASTProperties :: SetASTProperties.Request -> RPC Context IO SetASTProperties.Update
setASTProperties request@(SetASTProperties.Request tproperties tnodeID tlibID tprojectID) = do
    properties <- decodeE tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchP.setProperties properties nodeID libID projectID
    SetASTProperties.Update request <$> Batch.getUpdateNo


getNodeProperties :: GetNodeProperties.Request -> RPC Context IO GetNodeProperties.Status
getNodeProperties request@(GetNodeProperties.Request tnodeID _ tlibID tprojectID _) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- BatchP.getProperties nodeID libID projectID
    return $ GetNodeProperties.Status request (encode properties)


setNodeProperties :: SetNodeProperties.Request -> RPC Context IO SetNodeProperties.Update
setNodeProperties request@(SetNodeProperties.Request tproperties tnodeID _ tlibID tprojectID _) = do
    properties <- decodeE tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchP.setProperties properties nodeID libID projectID
    SetNodeProperties.Update request <$> Batch.getUpdateNo
