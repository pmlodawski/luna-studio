---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Project where

import qualified Data.IORef    as IORef
import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Handler.Project                            as BatchP
import qualified Flowbox.Batch.Process.Map                                as ProcessMap
import           Flowbox.Batch.Project.Project                            (Project)
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                           (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Close.Request     as Close
import qualified Generated.Proto.ProjectManager.Project.Close.Update      as Close
import qualified Generated.Proto.ProjectManager.Project.Create.Request    as Create
import qualified Generated.Proto.ProjectManager.Project.Create.Update     as Create
import qualified Generated.Proto.ProjectManager.Project.List.Request      as List
import qualified Generated.Proto.ProjectManager.Project.List.Status       as List
import qualified Generated.Proto.ProjectManager.Project.Lookup.Request    as Lookup
import qualified Generated.Proto.ProjectManager.Project.Lookup.Status     as Lookup
import qualified Generated.Proto.ProjectManager.Project.Modify.Request    as Modify
import qualified Generated.Proto.ProjectManager.Project.Modify.Update     as Modify
import qualified Generated.Proto.ProjectManager.Project.Open.Request      as Open
import qualified Generated.Proto.ProjectManager.Project.Open.Update       as Open
import qualified Generated.Proto.ProjectManager.Project.Store.Request     as Store
import qualified Generated.Proto.ProjectManager.Project.Store.Status      as Store



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handler.Project"

------ public api -------------------------------------------------


list :: ContextRef -> List.Request -> IO List.Status
list ctx _ = do
    batch <- IORef.readIORef ctx
    let aprojects       = BatchP.projects batch
        tprojects       = map (\a -> encode a ^. _1) aprojects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Status tprojectsVector


lookup :: ContextRef -> Lookup.Request -> IO Lookup.Status
lookup ctx (Lookup.Request tprojectID) = do
    let projectID = decodeP tprojectID
    batch     <- IORef.readIORef ctx
    project   <- BatchP.projectByID projectID batch
    return $ Lookup.Status $ encode (projectID, project) ^. _1


create :: ContextRef -> Create.Request -> IO Create.Update
create ctx (Create.Request tname tpath tattributes) = do
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    batch <- IORef.readIORef ctx
    (newBatch, newProject) <- BatchP.createProject name path attributes batch
    IORef.writeIORef ctx newBatch
    return $ Create.Update $ encode newProject ^. _1


open :: ContextRef -> Open.Request -> IO Open.Update
open ctx (Open.Request tpath) = do
    let upath = decodeP tpath
    batch <- IORef.readIORef ctx
    (newBatch, (projectID, project)) <- BatchP.openProject upath batch
    IORef.writeIORef ctx newBatch
    return $ Open.Update $ encode (projectID, project) ^. _1


modify :: ContextRef -> Modify.Request -> IO Modify.Update
modify ctx (Modify.Request tproject) = do
    projectWithID <- decode (tproject, LibManager.empty, ProcessMap.empty) :: IO (Project.ID, Project)
    batch         <- IORef.readIORef ctx
    newBatch      <-  BatchP.updateProject projectWithID batch
    IORef.writeIORef ctx newBatch
    return $ Modify.Update tproject


close :: ContextRef -> Close.Request -> IO Close.Update
close ctx (Close.Request tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    let newBatch = BatchP.closeProject projectID batch
    IORef.writeIORef ctx newBatch
    return $ Close.Update tprojectID


store :: ContextRef -> Store.Request -> IO Store.Status
store ctx (Store.Request tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    BatchP.storeProject projectID batch
    return $ Store.Status tprojectID
