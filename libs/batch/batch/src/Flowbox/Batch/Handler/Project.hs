---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Project where

import           Flowbox.Batch.Batch                  (Batch)
import qualified Flowbox.Batch.Handler.Common         as Batch
import           Flowbox.Batch.Project.Project        (Project)
import qualified Flowbox.Batch.Project.Project        as Project
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import qualified Flowbox.Batch.Project.Serialize      as Serialize
import           Flowbox.Prelude
import           Flowbox.System.UniPath               (UniPath)
import qualified Flowbox.System.UniPath               as UniPath
import           Luna.DEP.Graph.Attributes            (Attributes)
import qualified Luna.DEP.Lib.Lib                     as Library



projects :: Batch [(Project.ID, Project)]
projects = ProjectManager.labNodes <$> Batch.getProjectManager


projectByID :: Project.ID -> Batch Project
projectByID = Batch.getProject


createProject :: Maybe String -> UniPath -> Attributes -> Batch (Project.ID, Project)
createProject name path attributes = Batch.projectManagerOp $ \projectManager -> do
    expandedPath <- UniPath.expand path
    let project            = Project.make name expandedPath attributes
        (newpm, projectID) = ProjectManager.insNewNode project projectManager
    return (newpm, (projectID, project))


openProject :: UniPath -> Batch (Project.ID, Project)
openProject path = Batch.projectManagerOp $ \projectManager -> do
    expandedPath <- UniPath.expand path
    liftIO $ ProjectManager.openProject projectManager expandedPath


updateProject :: (Project.ID, Project) -> Batch ()
updateProject (projectID, project) = Batch.projectOp projectID $ \oldProject -> do
    let libs = oldProject ^. Project.libs
        newProject = project & Project.libs .~ libs
    return (newProject, ())


closeProject :: Project.ID -> Batch ()
closeProject projectID = Batch.projectManagerOp $ \projectManager ->
    return (ProjectManager.delNode projectID projectManager, ())


storeProject :: Project.ID -> [Library.ID] -> Maybe UniPath -> Batch ()
storeProject projectID libIDs mpath = do
    project <- Batch.getProject projectID
    liftIO $ Serialize.storeProject project libIDs mpath
