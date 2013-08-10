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
    setActiveProject
) where
    
import           Data.IORef                                                  
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)


import           Flowbox.Batch.Server.Handlers.Common                        
import qualified Projects_Types                                            as TProjects
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.Project                               (Project(..))
import qualified Flowbox.Luna.Core                                         as Core
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects    ()


------ public api helpers -----------------------------------------
projectOperation :: (IORef Batch -> (Project.ID, Project) -> a)
                 ->  IORef Batch -> Maybe TProjects.Project -> a
projectOperation operation batchHandler mtproject = case mtproject of 
    Nothing       -> throw' "`project` argument is missing";
    Just tproject -> case decode (tproject, Core.empty) of
        Left  message      -> throw' message
        Right (projectID, project) -> operation batchHandler (projectID, project)


------ public api -------------------------------------------------

projects :: IORef Batch -> IO (Vector TProjects.Project)
projects batchHandler = do
    batch <- readIORef batchHandler
    let aprojects       = Batch.projects batch
        tprojectsWCore  = map encode aprojects
        tprojects       = map (\(p, _) -> p) tprojectsWCore
        tprojectsVector = Vector.fromList tprojects
    return tprojectsVector


createProject :: IORef Batch -> Maybe TProjects.Project -> IO ()
createProject = projectOperation (\ batchHandler (_, project) -> do
    batch <- readIORef batchHandler
    Batch.createProject project batch)


openProject :: IORef Batch -> Maybe TProjects.Project -> IO TProjects.Project
openProject = projectOperation (\ batchHandler (_, project) -> do
    batch <- readIORef batchHandler
    (newBatch, (projectID, aproject)) <- Batch.openProject project batch
    writeIORef batchHandler newBatch
    let (tproject, _) = encode (projectID, aproject)
    return tproject)



closeProject :: IORef Batch -> Maybe TProjects.Project -> IO ()
closeProject = projectOperation (\ batchHandler (projectID, _) -> do
    batch <- readIORef batchHandler
    newBatch <- Batch.closeProject projectID batch
    writeIORef batchHandler newBatch)


storeProject :: IORef Batch -> Maybe TProjects.Project -> IO ()
storeProject = projectOperation (\ batchHandler (projectID, _) -> do
    batch <- readIORef batchHandler
    Batch.storeProject projectID batch)


setActiveProject :: IORef Batch -> Maybe TProjects.Project -> IO ()
setActiveProject = projectOperation (\ batchHandler (projectID, _) -> do
    batch <- readIORef batchHandler
    let newBatch = Batch.setActiveProject projectID batch
    writeIORef batchHandler newBatch)
