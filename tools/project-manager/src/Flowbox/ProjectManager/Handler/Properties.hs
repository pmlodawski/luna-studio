---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Properties where

import Data.IORef (IORef)

import qualified Data.IORef                                                               as IORef
import qualified Flowbox.Batch.Handler.Properties                                         as BatchP
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes                 ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                           (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Args   as GetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Get.Result as GetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Args   as SetProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Result as SetProperties



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Properties"


get :: ContextRef -> GetProperties.Args -> IO GetProperties.Result
get ctxRef (GetProperties.Args tnodeID tlibID tprojectID) = do
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    properties <- BatchP.getProperties nodeID libID projectID batch
    return $ GetProperties.Result $ encode properties


set :: ContextRef -> SetProperties.Args -> IO SetProperties.Result
set ctxRef (SetProperties.Args tproperties tnodeID tlibID tprojectID) = do
    properties <- decode tproperties
    let nodeID    = decodeP tnodeID
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchP.setProperties properties nodeID libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return SetProperties.Result
