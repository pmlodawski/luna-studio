---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.NodeDefault where

import qualified Data.IORef                                                                                   as IORef
import qualified Flowbox.Batch.Handler.NodeDefault                                                            as BatchND
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                                          ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.NodeDefault                                    ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                                               (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Args      as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Result    as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Args   as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Result as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Args      as NodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Result    as NodeDefaultSet



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handler.NodeDefault"


get :: ContextRef -> NodeDefaultGet.Args -> IO NodeDefaultGet.Result
get ctxRef (NodeDefaultGet.Args tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    nd <- BatchND.nodeDefaults nodeID bc libID projectID batch
    return $ NodeDefaultGet.Result $ encode nd


set :: ContextRef -> NodeDefaultSet.Args -> IO NodeDefaultSet.Result
set ctxRef (NodeDefaultSet.Args tdstPort tvalue tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        value     = decodeP tvalue
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchND.setNodeDefault dstPort value nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return NodeDefaultSet.Result


remove :: ContextRef -> NodeDefaultRemove.Args -> IO NodeDefaultRemove.Result
remove ctxRef (NodeDefaultRemove.Args tdstPort tnodeID tbc tlibID tprojectID) = do
    bc <- decode tbc
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchND.removeNodeDefault dstPort nodeID bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return NodeDefaultRemove.Result
