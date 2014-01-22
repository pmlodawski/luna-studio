---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Project.ProjectManager (
    module Flowbox.Data.Graph,
    ProjectManager,
    empty,
    openProject,
) where

import           Flowbox.Batch.Project.Project               (Project)
import qualified Flowbox.Batch.Project.Project               as Project
import qualified Flowbox.Batch.Tools.Serialize.Proto.Project as ProjectSerialization
import           Flowbox.Data.Graph                          hiding (Edge, Graph, empty)
import qualified Flowbox.Data.Graph                          as DG
import           Flowbox.Prelude                             hiding (empty)
import           Flowbox.System.UniPath                      (UniPath)



type ProjectManager = DG.Graph Project ()


empty :: ProjectManager
empty = DG.empty


openProject :: ProjectManager -> UniPath -> IO (ProjectManager, (Project.ID, Project))
openProject projectManager ppath = do
    project <- ProjectSerialization.restoreProject ppath
    let (newProjectManager, projectID) = insNewNode project projectManager
    return (newProjectManager, (projectID, project))


