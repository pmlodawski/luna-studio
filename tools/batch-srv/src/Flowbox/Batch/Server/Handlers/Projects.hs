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
import qualified Flowbox.Luna.Lib.LibManager                               as LibManager
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs         ()



------ public api -------------------------------------------------

projects :: IORef Project -> IO (Vector TProjects.Project)
projects batchHandler = undefined 
    --putStrLn "call libraries"
    --project <- readIORef batchHandler
    --let core        = Project.core project
    --    libManager' = Core.libManager core
    --    libs        = LibManager.labNodes libManager'
    --    tlibs       = map encode libs
    --    tlibsVector = Vector.fromList tlibs
    --return tlibsVector


createProject :: IORef Project -> Maybe TProjects.Project -> IO TProjects.Project
createProject = undefined --libOperation (\ batchHandler (_, library) -> do
    --putStrLn "call createLibrary - NOT YET IMPLEMENTED"
    --return $ encode (-1, library))


openProject :: IORef Project -> Maybe TProjects.Project -> IO TProjects.Project
openProject = undefined --libOperation (\ batchHandler (_, library) -> do
    --putStrLn "call loadLibrary"
    --project <- readIORef batchHandler
    --let core        = Project.core project
    --    (newCore, newLibrary, newLibID) = Core.loadLibrary core library
    --    newTLibrary = encode (newLibID, newLibrary)
    --    newProject = project{Project.core = newCore}
    --writeIORef batchHandler newProject
    --return newTLibrary)


closeProject :: IORef Project -> Maybe TProjects.Project -> IO ()
closeProject = undefined --libOperation (\ batchHandler (libID, _) -> do
    --putStrLn "call unloadLibrary"
    --project <- readIORef batchHandler
    --let core       = Project.core project
    --    newCore = Core.unloadLibrary core libID
    --    newProject = project{Project.core = newCore}
    --writeIORef batchHandler newProject)


storeProject :: IORef Project -> Maybe TProjects.Project -> IO ()
storeProject = undefined --libOperation (\ batchHandler (libID, _) -> do
    --putStrLn "call storeLibrary - NOT YET IMPLEMENTED")


setActiveProject :: IORef Project -> Maybe TProjects.Project -> IO ()
setActiveProject = undefined
