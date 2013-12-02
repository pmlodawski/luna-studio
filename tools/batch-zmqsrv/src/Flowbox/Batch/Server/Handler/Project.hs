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
import qualified Data.Sequence                                          as Sequence
import           Flowbox.Prelude                                          
import           Flowbox.Batch.Batch                                      (Batch(..))
import qualified Flowbox.Batch.Handler.Project                          as BatchP
import qualified Flowbox.Batch.Project.Project                          as Project
import           Flowbox.Batch.Project.Project                            (Project(..))
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import           Flowbox.Control.Error                                    
import qualified Flowbox.Luna.Lib.LibManager                            as LibManager
import           Flowbox.System.Log.Logger                                
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic           
import qualified Generated.Proto.Batch.Project.Projects.Args            as Projects
import qualified Generated.Proto.Batch.Project.Projects.Result          as Projects
import qualified Generated.Proto.Batch.Project.ProjectByID.Args         as ProjectByID
import qualified Generated.Proto.Batch.Project.ProjectByID.Result       as ProjectByID
import qualified Generated.Proto.Batch.Project.CreateProject.Args       as CreateProject
import qualified Generated.Proto.Batch.Project.CreateProject.Result     as CreateProject
import qualified Generated.Proto.Batch.Project.OpenProject.Args         as OpenProject
import qualified Generated.Proto.Batch.Project.OpenProject.Result       as OpenProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Args       as UpdateProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Result     as UpdateProject
import qualified Generated.Proto.Batch.Project.CloseProject.Args        as CloseProject
import qualified Generated.Proto.Batch.Project.CloseProject.Result      as CloseProject
import qualified Generated.Proto.Batch.Project.StoreProject.Args        as StoreProject
import qualified Generated.Proto.Batch.Project.StoreProject.Result      as StoreProject



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.Project"

------ public api -------------------------------------------------


projects :: IORef Batch -> Projects.Args -> Script Projects.Result
projects batchHandler _ = do
    scriptIO $ loggerIO info "called projects"
    batch <- tryReadIORef batchHandler
    let aprojects       = BatchP.projects batch
        tprojects       = map (fst . encode) aprojects
        tprojectsVector = Sequence.fromList tprojects
    return $ Projects.Result tprojectsVector


projectByID :: IORef Batch -> ProjectByID.Args -> Script ProjectByID.Result
projectByID batchHandler (ProjectByID.Args tprojectID) = do
    scriptIO $ loggerIO info "called projectByID"
    let projectID = decodeP tprojectID
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    batch     <- tryReadIORef batchHandler
    project   <- tryRight $ BatchP.projectByID projectID batch
    return $ ProjectByID.Result $ fst $ encode (projectID, project)


createProject :: IORef Batch -> CreateProject.Args -> Script CreateProject.Result
createProject batchHandler (CreateProject.Args tproject) = do
    scriptIO $ loggerIO info "called createProject"
    (_, project) <- tryRight (decode (tproject, LibManager.empty) :: Either String (Project.ID, Project))
    batch        <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "project: " ++ (show project)
    let (newBatch, newProject) = BatchP.createProject project batch
    tryWriteIORef batchHandler newBatch
    return $ CreateProject.Result $ fst $ encode newProject


openProject :: IORef Batch -> OpenProject.Args -> Script OpenProject.Result
openProject batchHandler (OpenProject.Args tpath) = do
    scriptIO $ loggerIO info "called openProject"
    let upath = decodeP tpath
    batch <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    (newBatch, (projectID, aproject)) <- scriptIO $ BatchP.openProject upath batch
    tryWriteIORef batchHandler newBatch
    return $ OpenProject.Result $ fst $ encode (projectID, aproject)


updateProject :: IORef Batch -> UpdateProject.Args -> Script UpdateProject.Result
updateProject batchHandler  (UpdateProject.Args tproject) = do
    scriptIO $ loggerIO info "called updateProject"
    project <- tryRight (decode (tproject, LibManager.empty) :: Either String (Project.ID, Project))
    batch   <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "project: " ++ (show project)
    newBatch <- tryRight $  BatchP.updateProject project batch
    tryWriteIORef batchHandler newBatch
    return UpdateProject.Result


closeProject :: IORef Batch -> CloseProject.Args -> Script CloseProject.Result
closeProject batchHandler (CloseProject.Args tprojectID) = do
    scriptIO $ loggerIO info "called closeProject"
    let projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    let newBatch = BatchP.closeProject projectID batch
    tryWriteIORef batchHandler newBatch
    return CloseProject.Result


storeProject :: IORef Batch -> StoreProject.Args -> Script StoreProject.Result
storeProject batchHandler (StoreProject.Args tprojectID) = do
    scriptIO $ loggerIO info "called storeProject"
    let projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    scriptIO $ BatchP.storeProject projectID batch
    return StoreProject.Result
