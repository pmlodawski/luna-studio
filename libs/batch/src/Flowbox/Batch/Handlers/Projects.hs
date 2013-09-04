---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Projects (
    projects,

    projectByID,
    createProject,
    openProject,
    updateProject,
    closeProject,
    storeProject,
) where


import           Flowbox.Batch.Batch                     (Batch(..))
import           Flowbox.Batch.Handlers.Common           (noresult, readonly, readonly', projectOp, projectOp')
import qualified Flowbox.Batch.Project.Project         as Project
import           Flowbox.Batch.Project.Project           (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager  as ProjectManager
import qualified Flowbox.Batch.Tools.Serialize.Project as ProjectSerialization
import           Flowbox.System.UniPath                  (UniPath)
import qualified Flowbox.Batch.Samples.Modules         as Samples



projects :: Batch -> [(Project.ID, Project)]
projects batch = ProjectManager.labNodes (projectManager batch)


projectByID :: Project.ID -> Batch -> Either String Project
projectByID projectID = readonly . projectOp projectID (\_ project -> do
    return (project, project))


createProject :: Project -> Batch -> (Batch, (Project.ID, Project))
createProject project batch = (newBatch, (projectID, project)) where
    pm                 = projectManager batch
    projectWithLibs    = Samples.addDefaultLibraries project
    (newpm, projectID) = ProjectManager.insNewNode projectWithLibs pm
    newBatch           = batch { projectManager = newpm }


openProject :: UniPath -> Batch -> IO (Batch, (Project.ID, Project))
openProject ppath batch = do
    let aprojectManager = projectManager batch
    (newProjectManager, newP) <- ProjectManager.openProject aprojectManager ppath
    let newBatch = batch {projectManager = newProjectManager}
    return (newBatch, newP)


updateProject :: (Project.ID, Project) -> Batch -> Either String Batch
updateProject (projectID, project) = noresult . projectOp projectID (\_ oldProject -> do
    let plibs = Project.libs oldProject
        newProject = project { Project.libs = plibs }
    return (newProject, ()))


closeProject :: Project.ID -> Batch -> Batch
closeProject projectID batch = newBatch where 
    aprojectManager   = projectManager batch
    newProjectManager = ProjectManager.delNode projectID aprojectManager
    newBatch          = batch {projectManager = newProjectManager}


storeProject :: Project.ID -> Batch -> IO ()
storeProject projectID = readonly' . projectOp' projectID (\_ project -> do
    ProjectSerialization.storeProject project
    return (project, ()))

