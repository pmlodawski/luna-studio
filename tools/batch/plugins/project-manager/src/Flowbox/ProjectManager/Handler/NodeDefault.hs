---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.NodeDefault where

import qualified Data.IORef                                                                                    as IORef
import qualified Flowbox.Batch.Handler.NodeDefault                                                             as BatchND
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                                           ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.NodeDefault                                     ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                                (ContextRef)
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


get :: ContextRef -> NodeDefaultGet.Request -> IO NodeDefaultGet.Status
get ctxRef (NodeDefaultGet.Request tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    nodeDefaults <- BatchND.nodeDefaults nodeID bc libID projectID batch
    return $ NodeDefaultGet.Status (encode nodeDefaults) tnodeID tbc tlibID tprojectID


set :: ContextRef -> NodeDefaultSet.Request -> IO NodeDefaultSet.Update
set ctxRef (NodeDefaultSet.Request tdstPort tvalue tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        value     = decodeP tvalue
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchND.setNodeDefault dstPort value nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeDefaultSet.Update tdstPort tvalue tnodeID tbc tlibID tprojectID


remove :: ContextRef -> NodeDefaultRemove.Request -> IO NodeDefaultRemove.Update
remove ctxRef (NodeDefaultRemove.Request tdstPort tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchND.removeNodeDefault dstPort nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ NodeDefaultRemove.Update tdstPort tnodeID tbc tlibID tprojectID
