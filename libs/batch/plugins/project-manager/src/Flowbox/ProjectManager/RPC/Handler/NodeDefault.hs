---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.NodeDefault where

import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.NodeDefault                                                             as BatchND
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Prelude                                                                               hiding (Context)
import           Flowbox.ProjectManager.Context                                                                (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Request    as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Get.Status     as NodeDefaultGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Request as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Update  as NodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Request    as NodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Update     as NodeDefaultSet
import           Luna.Data.Serialize.Proto.Conversion.Crumb                                                    ()
import           Luna.Data.Serialize.Proto.Conversion.Graph                                                    ()
import           Luna.Data.Serialize.Proto.Conversion.NodeDefault                                              ()



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


get :: NodeDefaultGet.Request -> RPC Context IO NodeDefaultGet.Status
get request@(NodeDefaultGet.Request tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    nodeDefaults <- BatchND.nodeDefaults nodeID bc libID projectID
    return $ NodeDefaultGet.Status request (encode nodeDefaults)


set :: NodeDefaultSet.Request -> RPC Context IO NodeDefaultSet.Update
set request@(NodeDefaultSet.Request tdstPort tvalue tnodeID tbc tlibID tprojectID _) = do
    bc    <- decodeE tbc
    value <- decodeE tvalue
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchND.setNodeDefault dstPort value nodeID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeDefaultSet.Update request updateNo


remove :: NodeDefaultRemove.Request -> RPC Context IO NodeDefaultRemove.Update
remove request@(NodeDefaultRemove.Request tdstPort tnodeID tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let dstPort   = decodeListP tdstPort
        nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchND.removeNodeDefault dstPort nodeID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ NodeDefaultRemove.Update request updateNo
