---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Project (
    projects,

    projectByID,
    createProject,
    openProject,
    updateProject,
    closeProject,
    storeProject,
) where

import           Data.IORef                                               (IORef)
import qualified Data.IORef                                               as IORef
import qualified Data.Sequence                                            as Sequence
import           Flowbox.Batch.Batch                                      (Batch)
import qualified Flowbox.Batch.Handler.Project                            as BatchP
import           Flowbox.Batch.Project.Project                            (Project (Project))
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Project.CloseProject.Args          as CloseProject
import qualified Generated.Proto.Batch.Project.CloseProject.Result        as CloseProject
import qualified Generated.Proto.Batch.Project.CreateProject.Args         as CreateProject
import qualified Generated.Proto.Batch.Project.CreateProject.Result       as CreateProject
import qualified Generated.Proto.Batch.Project.OpenProject.Args           as OpenProject
import qualified Generated.Proto.Batch.Project.OpenProject.Result         as OpenProject
import qualified Generated.Proto.Batch.Project.ProjectByID.Args           as ProjectByID
import qualified Generated.Proto.Batch.Project.ProjectByID.Result         as ProjectByID
import qualified Generated.Proto.Batch.Project.Projects.Args              as Projects
import qualified Generated.Proto.Batch.Project.Projects.Result            as Projects
import qualified Generated.Proto.Batch.Project.StoreProject.Args          as StoreProject
import qualified Generated.Proto.Batch.Project.StoreProject.Result        as StoreProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Args         as UpdateProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Result       as UpdateProject



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.Project"

------ public api -------------------------------------------------


projects :: IORef Batch -> Projects.Args -> IO Projects.Result
projects batchHandler _ = do
    loggerIO info "called projects"
    batch <- IORef.readIORef batchHandler
    let aprojects       = BatchP.projects batch
        tprojects       = map (fst . encode) aprojects
        tprojectsVector = Sequence.fromList tprojects
    return $ Projects.Result tprojectsVector


projectByID :: IORef Batch -> ProjectByID.Args -> IO ProjectByID.Result
projectByID batchHandler (ProjectByID.Args tprojectID) = do
    loggerIO info "called projectByID"
    let projectID = decodeP tprojectID
    loggerIO debug $ "projectID: " ++ (show projectID)
    batch     <- IORef.readIORef batchHandler
    project   <- BatchP.projectByID projectID batch
    return $ ProjectByID.Result $ fst $ encode (projectID, project)


createProject :: IORef Batch -> CreateProject.Args -> IO CreateProject.Result
createProject batchHandler (CreateProject.Args tname tpath tattributes) = do
    loggerIO info "called createProject"
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    batch        <- IORef.readIORef batchHandler
    loggerIO debug $ "name: " ++ (show name) ++ " path: " ++ (show path) ++ " attributes: " ++ (show attributes)
    let (newBatch, newProject) = BatchP.createProject name path attributes batch
    IORef.writeIORef batchHandler newBatch
    return $ CreateProject.Result $ fst $ encode newProject


openProject :: IORef Batch -> OpenProject.Args -> IO OpenProject.Result
openProject batchHandler (OpenProject.Args tpath) = do
    loggerIO info "called openProject"
    let upath = decodeP tpath
    batch <- IORef.readIORef batchHandler
    loggerIO debug $ "path: " ++ (show upath)
    (newBatch, (projectID, aproject)) <- BatchP.openProject upath batch
    IORef.writeIORef batchHandler newBatch
    return $ OpenProject.Result $ fst $ encode (projectID, aproject)


updateProject :: IORef Batch -> UpdateProject.Args -> IO UpdateProject.Result
updateProject batchHandler  (UpdateProject.Args tproject) = do
    loggerIO info "called updateProject"
    project <- (decode (tproject, LibManager.empty) :: IO (Project.ID, Project))
    batch   <- IORef.readIORef batchHandler
    loggerIO debug $ "project: " ++ (show project)
    newBatch <-  BatchP.updateProject project batch
    IORef.writeIORef batchHandler newBatch
    return UpdateProject.Result


closeProject :: IORef Batch -> CloseProject.Args -> IO CloseProject.Result
closeProject batchHandler (CloseProject.Args tprojectID) = do
    loggerIO info "called closeProject"
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    loggerIO debug $ "projectID: " ++ (show projectID)
    let newBatch = BatchP.closeProject projectID batch
    IORef.writeIORef batchHandler newBatch
    return CloseProject.Result


storeProject :: IORef Batch -> StoreProject.Args -> IO StoreProject.Result
storeProject batchHandler (StoreProject.Args tprojectID) = do
    loggerIO info "called storeProject"
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    loggerIO debug $ "projectID: " ++ (show projectID)
    BatchP.storeProject projectID batch
    return StoreProject.Result
