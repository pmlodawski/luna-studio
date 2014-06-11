---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.NodeDefault where

import qualified Flowbox.Batch.Handler.NodeDefault                                                             as BatchND
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                                           ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.NodeDefault                                     ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                                (ContextRef)
import qualified Flowbox.ProjectManager.Context                                                                as Context
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Request    as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Status     as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Request as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Update  as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Request    as NodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Update     as NodeDefaultSet



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handler.NodeDefault"


get :: ContextRef -> NodeDefaultGet.Request -> RPC NodeDefaultGet.Status
get ctxRef (NodeDefaultGet.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    nodeDefaults <- Context.run ctxRef $ BatchND.nodeDefaults nodeID bc libID projectID
    return $ NodeDefaultGet.Status (encode nodeDefaults) tnodeID tbc tlibID tprojectID


set :: ContextRef -> NodeDefaultSet.Request -> RPC NodeDefaultSet.Update
set ctxRef (NodeDefaultSet.Request tdstPort tvalue tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        value     = decodeP tvalue
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchND.setNodeDefault dstPort value nodeID bc libID projectID
    return $ NodeDefaultSet.Update tdstPort tvalue tnodeID tbc tlibID tprojectID


remove :: ContextRef -> NodeDefaultRemove.Request -> RPC NodeDefaultRemove.Update
remove ctxRef (NodeDefaultRemove.Request tdstPort tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchND.removeNodeDefault dstPort nodeID bc libID projectID
    return $ NodeDefaultRemove.Update tdstPort tnodeID tbc tlibID tprojectID
