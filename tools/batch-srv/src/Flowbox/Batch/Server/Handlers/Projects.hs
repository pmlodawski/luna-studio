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
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)


import           Flowbox.Batch.Server.Handlers.Common
import qualified Projects_Types                                            as TProjects
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.Project                               (Project(..))
import qualified Flowbox.Luna.Core                                         as Core
import           Flowbox.Luna.Core                                           (Core(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects    ()


------ public api helpers -----------------------------------------
projectOperation :: (IORef Project -> (Project.ID, Project) -> a)
                 ->  IORef Project -> Maybe TProjects.Project -> a
projectOperation operation batchHandler mtproject = case mtproject of 
    Nothing       -> throw' "`project` argument is missing";
    Just tproject -> case decode (tproject, Core.empty) of
        Left  message      -> throw' message
        Right (projectID, project) -> operation batchHandler (projectID, project)


------ public api -------------------------------------------------

projects :: IORef Project -> IO (Vector TProjects.Project)
projects batchHandler = do
    putStrLn "NOT IMPLEMENTED"
    return undefined
    --putStrLn "call libraries"
    --project <- readIORef batchHandler
    --let core        = Project.core project
    --    libManager' = Core.libManager core
    --    libs        = LibManager.labNodes libManager'
    --    tlibs       = map encode libs
    --    tlibsVector = Vector.fromList tlibs
    --return tlibsVector


createProject :: IORef Project -> Maybe TProjects.Project -> IO TProjects.Project
createProject = projectOperation (\ batchHandler (projectID, project) -> do
    putStrLn "NOT IMPLEMENTED"
    return undefined) --libOperation (\ batchHandler (_, library) -> do
    --putStrLn "call createLibrary - NOT YET IMPLEMENTED"
    --return $ encode (-1, library))


openProject :: IORef Project -> Maybe TProjects.Project -> IO TProjects.Project
openProject = projectOperation (\ batchHandler (projectID, project) -> do
    putStrLn "NOT IMPLEMENTED"
    return undefined) --libOperation (\ batchHandler (_, library) -> do
    --putStrLn "call loadLibrary"
    --project <- readIORef batchHandler
    --let core        = Project.core project
    --    (newCore, newLibrary, newLibID) = Core.loadLibrary core library
    --    newTLibrary = encode (newLibID, newLibrary)
    --    newProject = project{Project.core = newCore}
    --writeIORef batchHandler newProject
    --return newTLibrary)


closeProject :: IORef Project -> Maybe TProjects.Project -> IO ()
closeProject = projectOperation (\ batchHandler (projectID, project) -> do
    putStrLn "NOT IMPLEMENTED") --libOperation (\ batchHandler (libID, _) -> do
    --putStrLn "call unloadLibrary"
    --project <- readIORef batchHandler
    --let core       = Project.core project
    --    newCore = Core.unloadLibrary core libID
    --    newProject = project{Project.core = newCore}
    --writeIORef batchHandler newProject)


storeProject :: IORef Project -> Maybe TProjects.Project -> IO ()
storeProject = projectOperation (\ batchHandler (projectID, project) -> do
    putStrLn "NOT IMPLEMENTED") --libOperation (\ batchHandler (libID, _) -> do
    --putStrLn "call storeLibrary - NOT YET IMPLEMENTED")


setActiveProject :: IORef Project -> Maybe TProjects.Project -> IO ()
setActiveProject = projectOperation (\ batchHandler (projectID, project) -> do
    putStrLn "NOT IMPLEMENTED")

