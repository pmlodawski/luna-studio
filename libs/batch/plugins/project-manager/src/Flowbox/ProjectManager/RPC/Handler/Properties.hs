---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Properties where

import qualified Flowbox.Batch.Handler.Properties                                                              as BatchP
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes                                      ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                                (ContextRef)
import qualified Flowbox.ProjectManager.Context                                                                as Context
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Properties"


getASTProperties :: ContextRef -> GetASTProperties.Request -> RPC IO GetASTProperties.Status
getASTProperties ctxRef request@(GetASTProperties.Request tnodeID tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- Context.run ctxRef $ BatchP.getProperties nodeID libID projectID
    return $ GetASTProperties.Status request $ encode properties


setASTProperties :: ContextRef -> SetASTProperties.Request -> RPC IO SetASTProperties.Update
setASTProperties ctxRef request@(SetASTProperties.Request tproperties tnodeID tlibID tprojectID) = do
    properties <- decodeE tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchP.setProperties properties nodeID libID projectID
    return $ SetASTProperties.Update request


getNodeProperties :: ContextRef -> GetNodeProperties.Request -> RPC IO GetNodeProperties.Status
getNodeProperties ctxRef request@(GetNodeProperties.Request tnodeID _ tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    properties <- Context.run ctxRef $ BatchP.getProperties nodeID libID projectID
    return $ GetNodeProperties.Status request (encode properties)


setNodeProperties :: ContextRef -> SetNodeProperties.Request -> RPC IO SetNodeProperties.Update
setNodeProperties ctxRef request@(SetNodeProperties.Request tproperties tnodeID _ tlibID tprojectID) = do
    properties <- decodeE tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchP.setProperties properties nodeID libID projectID
    return $ SetNodeProperties.Update request
