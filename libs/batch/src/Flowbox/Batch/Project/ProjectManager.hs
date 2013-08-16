---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Project.ProjectManager (
    module Flowbox.Luna.Data.Graph,
    ProjectManager,
    empty,
    openProject,
) where

import qualified Flowbox.Batch.Tools.Serialize.Project as ProjectSerialization
import           Flowbox.System.UniPath                  (UniPath)
import qualified Flowbox.Batch.Project.Project         as Project
import           Flowbox.Batch.Project.Project           (Project)
import           Flowbox.Luna.Data.Graph                  hiding(Graph, Edge, empty)
import qualified Flowbox.Luna.Data.Graph               as DG


type ProjectManager = DG.Graph Project ()

empty :: ProjectManager
empty = DG.empty


openProject :: ProjectManager -> UniPath -> IO (ProjectManager, (Project.ID, Project))
openProject projectManager ppath = do
    project <- ProjectSerialization.restoreProject ppath
    let (newProjectManager, projectID) = insNewNode project projectManager
    return (newProjectManager, (projectID, project))


