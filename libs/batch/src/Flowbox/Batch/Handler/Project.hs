---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Project (
    projects,

    projectByID,
    createProject,
    openProject,
    updateProject,
    closeProject,
    storeProject,
) where

import           Flowbox.Batch.Batch                         (Batch)
import qualified Flowbox.Batch.Batch                         as Batch
import           Flowbox.Batch.Handler.Common                (noresult, projectOp, readonly)
import           Flowbox.Batch.Project.Project               (Project)
import qualified Flowbox.Batch.Project.Project               as Project
import qualified Flowbox.Batch.Project.ProjectManager        as ProjectManager
import qualified Flowbox.Batch.Tools.Serialize.Proto.Project as ProjectSerialization
import           Flowbox.Luna.Data.Attributes                (Attributes)
import           Flowbox.Prelude
import           Flowbox.System.UniPath                      (UniPath)


projects :: Batch -> [(Project.ID, Project)]
projects batch = ProjectManager.labNodes (Batch.projectManager batch)


projectByID :: (Applicative m, Monad m) => Project.ID -> Batch -> m Project
projectByID projectID = readonly . projectOp projectID (\_ project -> do
    return (project, project))


createProject :: String -> UniPath -> Attributes -> Batch -> (Batch, (Project.ID, Project))
createProject name path attributes batch = (newBatch, (projectID, project)) where
    project            = Project.make name path attributes
    pm                 = Batch.projectManager batch
    (newpm, projectID) = ProjectManager.insNewNode project pm
    newBatch           = batch { Batch.projectManager = newpm }


openProject :: UniPath -> Batch -> IO (Batch, (Project.ID, Project))
openProject ppath batch = do
    let aprojectManager = Batch.projectManager batch
    (newProjectManager, newP) <- ProjectManager.openProject aprojectManager ppath
    let newBatch = batch {Batch.projectManager = newProjectManager}
    return (newBatch, newP)


updateProject :: (Applicative m, Monad m) => (Project.ID, Project) -> Batch -> m Batch
updateProject (projectID, project) = noresult . projectOp projectID (\_ oldProject -> do
    let libs = Project.libs oldProject
        newProject = project { Project.libs = libs }
    return (newProject, ()))


closeProject :: Project.ID -> Batch -> Batch
closeProject projectID batch = newBatch where
    projectManager    = Batch.projectManager batch
    newProjectManager = ProjectManager.delNode projectID projectManager
    newBatch          = batch {Batch.projectManager = newProjectManager}


storeProject :: Project.ID -> Batch -> IO ()
storeProject projectID = readonly . projectOp projectID (\_ project -> do
    ProjectSerialization.storeProject project
    return (project, ()))

