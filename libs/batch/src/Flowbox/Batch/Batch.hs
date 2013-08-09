---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Batch(
    Batch(..),
    empty
) where


import qualified Flowbox.Batch.Project.Project            as Project
import           Flowbox.Batch.Project.Project              (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager     as ProjectManager
import           Flowbox.Batch.Project.ProjectManager       (ProjectManager)
import qualified Flowbox.Luna.Core                        as Core
import           Flowbox.Luna.Core                          (Core(..))
import qualified Flowbox.Luna.Lib.LibManager              as LibManager
import           Flowbox.Luna.Lib.LibManager                (LibManager)
import qualified Flowbox.Luna.Lib.Library                 as Library
import           Flowbox.Luna.Lib.Library                   (Library(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.Luna.Network.Def.DefManager      as DefManager
import           Flowbox.Luna.Network.Def.DefManager        (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition      as Definition
import           Flowbox.Luna.Network.Def.Definition        (Definition(..))
import qualified Flowbox.Luna.Network.Flags               as Flags
import qualified Flowbox.Luna.Network.Graph.Graph         as Graph
import qualified Flowbox.System.UniPath                   as UniPath
import           Flowbox.System.UniPath                     (UniPath)
import qualified Flowbox.Luna.Type.Type                   as Type


data Batch = Batch { projectManager  :: ProjectManager
                   , activeProjectID :: Project.ID
                   }

empty :: Batch
empty = Batch ProjectManager.empty (-1)


-------- Projects -------------------------------------------------------------

projects :: Batch -> [(Project.ID, Project)]
projects batch = ProjectManager.projects (projectManager batch)



createProject :: Batch -> Project -> IO ()
createProject _ = ProjectManager.createProject



openProject :: Batch -> Project -> IO (Batch, (Project.ID, Project))
openProject batch project = do
    let aprojectManager = projectManager batch
    (newProjectManager, newP) <- ProjectManager.openProject aprojectManager project
    let newBatch = batch {projectManager = newProjectManager}
    return (newBatch, newP)



closeProject :: Batch -> Project.ID -> IO Batch
closeProject batch projectID = do
    let aprojectManager = projectManager batch
    newProjectManager <- ProjectManager.closeProject aprojectManager projectID
    let newBatch = batch {projectManager = newProjectManager}
    return newBatch



storeProject :: Batch -> Project.ID -> IO ()
storeProject batch projectID = ProjectManager.storeProject (projectManager batch) projectID



setActiveProject :: Batch -> Project.ID -> Batch
setActiveProject batch projectID = batch { activeProjectID = projectID }


-------- Libraries ------------------------------------------------------------


-------- Definitions ----------------------------------------------------------


-------- Graphs ---------------------------------------------------------------