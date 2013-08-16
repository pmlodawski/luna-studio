---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Projects (
    projects,

    createProject,
    openProject, 
    closeProject,
    storeProject,
    setActiveProject,
    activeProject
) where


import           Data.Int                                                   
import           Data.IORef                                                 
import qualified Data.Vector                                              as Vector
import           Data.Vector                                                (Vector)
import           Data.Text.Lazy                                             (Text)

import qualified Projects_Types                                           as TProjects
import           Flowbox.Control.Error                                      
import qualified Flowbox.Batch.Batch                                      as Batch
import           Flowbox.Batch.Batch                                        (Batch(..))
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Project.Project                              (Project(..))
import           Flowbox.Batch.Server.Handlers.Common                       
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.Tools.Conversion                                   

------ public api -------------------------------------------------

projects :: IORef Batch -> IO (Vector TProjects.Project)
projects batchHandler = do
    putStrLn "call projects"

    batch <- readIORef batchHandler
    let aprojects       = Batch.projects batch
        tprojects       = map (fst . encode) aprojects
        tprojectsVector = Vector.fromList tprojects
    return tprojectsVector


createProject :: IORef Batch -> Maybe TProjects.Project -> IO TProjects.Project
createProject batchHandler mtproject = tRunScript $ do
    scriptIO $ putStrLn "call createProject"

    tproject     <- mtproject <??> "`project` field is missing" 
    (_, project) <- tryRight (decode (tproject, LibManager.empty) :: Either String (Project.ID, Project))
    
    batch        <- tryReadIORef batchHandler
    let (newBatch, newProject) = Batch.createProject project batch
    tryWriteIORef batchHandler newBatch

    return $ fst $ encode newProject


openProject :: IORef Batch -> Maybe Text -> IO TProjects.Project
openProject batchHandler mtpath = tRunScript $ do
    scriptIO $ putStrLn "call openProject"

    upath  <- tryGetUniPath mtpath "path"
    
    batch <- tryReadIORef batchHandler
    (newBatch, (projectID, aproject)) <- scriptIO $ Batch.openProject upath batch
    tryWriteIORef batchHandler newBatch

    return $ fst $ encode (projectID, aproject)


closeProject :: IORef Batch -> Maybe Int32 -> IO ()
closeProject batchHandler mtprojectID = tRunScript $ do
    scriptIO $ putStrLn "call closeProject"

    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler

    let newBatch = Batch.closeProject projectID batch
    tryWriteIORef batchHandler newBatch


storeProject :: IORef Batch -> Maybe Int32 -> IO ()
storeProject batchHandler mtprojectID = tRunScript $ do
    scriptIO $ putStrLn "call storeProject"

    projectID <- tryGetID mtprojectID "projectID"

    batch <- tryReadIORef batchHandler
    scriptIO $ Batch.storeProject projectID batch


setActiveProject :: IORef Batch -> Maybe Int32 -> IO ()
setActiveProject batchHandler mtprojectID = tRunScript $ do
    scriptIO $ putStrLn "call setActiveProject"

    projectID <- tryGetID mtprojectID "projectID"

    batch <- tryReadIORef batchHandler
    let newBatch = Batch.setActiveProject projectID batch
    tryWriteIORef batchHandler newBatch


activeProject :: IORef Batch -> IO TProjects.Project
activeProject batchHandler = tRunScript $ do
    scriptIO $ putStrLn "call activeProject"

    batch <- tryReadIORef batchHandler
    project <- (Batch.activeProject batch) <??> "No active project set"
    let projectID = Batch.activeProjectID batch
    return $ fst $ encode (projectID, project)
