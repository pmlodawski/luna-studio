---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Properties where

import qualified Data.IORef                                                                as IORef
import qualified Flowbox.Batch.Handler.Properties                                          as BatchP
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes                  ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                            (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Request as GetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Status  as GetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Request as SetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Update  as SetProperties



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Properties"


get :: ContextRef -> GetProperties.Request -> IO GetProperties.Status
get ctxRef (GetProperties.Request tnodeID tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    properties <- BatchP.getProperties nodeID libID projectID batch
    return $ GetProperties.Status (encode properties) tnodeID tlibID tprojectID


set :: ContextRef -> SetProperties.Request -> IO SetProperties.Update
set ctxRef (SetProperties.Request tproperties tnodeID tlibID tprojectID) = do
    properties <- decode tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchP.setProperties properties nodeID libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ SetProperties.Update tproperties tnodeID tlibID tprojectID
