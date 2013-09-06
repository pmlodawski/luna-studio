---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Projects (
    projects,

    projectByID,
    createProject,
    openProject, 
    updateProject,
    closeProject,
    storeProject,
) where


import           Data.Int                                                   (Int32)
import           Data.IORef                                                 
import qualified Data.Vector                                              as Vector
import           Data.Vector                                                (Vector)
import           Data.Text.Lazy                                             (Text)

import qualified Projects_Types                                           as TProjects
import           Flowbox.Control.Error                                      
import           Flowbox.Batch.Batch                                        (Batch(..))
import qualified Flowbox.Batch.Handlers.Projects                          as BatchP
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Project.Project                              (Project(..))
import           Flowbox.Batch.Server.Handlers.Common                       (tRunScript)
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.System.Log.Logger                                  
import           Flowbox.Tools.Conversion                                   



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Projects"

------ public api -------------------------------------------------

projects :: IORef Batch -> IO (Vector TProjects.Project)
projects batchHandler = do
    loggerIO info "called projects"
    batch <- readIORef batchHandler
    let aprojects       = BatchP.projects batch
        tprojects       = map (fst . encode) aprojects
        tprojectsVector = Vector.fromList tprojects
    return tprojectsVector


projectByID :: IORef Batch -> Maybe Int32 -> IO TProjects.Project
projectByID batchHandler mtprojectID = tRunScript $ do
    scriptIO $ loggerIO info "called projectByID"
    projectID <- tryGetID mtprojectID "projectID"
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    batch     <- tryReadIORef batchHandler
    project   <- tryRight $ BatchP.projectByID projectID batch
    return $ fst $ encode (projectID, project)


createProject :: IORef Batch -> Maybe TProjects.Project -> IO TProjects.Project
createProject batchHandler mtproject = tRunScript $ do
    scriptIO $ loggerIO info "called createProject"
    tproject     <- mtproject <??> "'project' field is missing" 
    (_, project) <- tryRight (decode (tproject, LibManager.empty) :: Either String (Project.ID, Project))
    batch        <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "project: " ++ (show project)
    let (newBatch, newProject) = BatchP.createProject project batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode newProject


openProject :: IORef Batch -> Maybe Text -> IO TProjects.Project
openProject batchHandler mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called openProject"
    upath <- tryGetUniPath mtpath "path"
    batch <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    (newBatch, (projectID, aproject)) <- scriptIO $ BatchP.openProject upath batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode (projectID, aproject)


updateProject :: IORef Batch -> Maybe TProjects.Project -> IO ()
updateProject batchHandler mtproject = tRunScript $ do
    scriptIO $ loggerIO info "called updateProject"
    tproject <- mtproject <??> "'project' field is missing" 
    project <- tryRight (decode (tproject, LibManager.empty) :: Either String (Project.ID, Project))
    batch    <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "project: " ++ (show project)
    newBatch <- tryRight $  BatchP.updateProject project batch
    tryWriteIORef batchHandler newBatch


closeProject :: IORef Batch -> Maybe Int32 -> IO ()
closeProject batchHandler mtprojectID = tRunScript $ do
    scriptIO $ loggerIO info "called closeProject"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    let newBatch = BatchP.closeProject projectID batch
    tryWriteIORef batchHandler newBatch


storeProject :: IORef Batch -> Maybe Int32 -> IO ()
storeProject batchHandler mtprojectID = tRunScript $ do
    scriptIO $ loggerIO info "called storeProject"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    scriptIO $ BatchP.storeProject projectID batch


