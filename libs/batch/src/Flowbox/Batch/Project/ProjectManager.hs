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
	projects,
	createProject,
	openProject,
	closeProject,
	storeProject
) where

import qualified Flowbox.Batch.Project.Project as Project
import           Flowbox.Batch.Project.Project    (Project)

import           Flowbox.Luna.Data.Graph         hiding(Graph, Edge, empty)
import qualified Flowbox.Luna.Data.Graph         as DG


type ProjectManager = DG.Graph Project ()

empty :: ProjectManager
empty = DG.empty


projects :: ProjectManager -> [(Project.ID, Project)]
projects = error "Not implemented"


createProject :: Project -> IO () 
createProject = error "Not implemented"


openProject :: ProjectManager -> Project -> IO (ProjectManager, (Project.ID, Project))
openProject = error "Not implemented"


closeProject :: ProjectManager -> Project.ID -> IO ProjectManager
closeProject = error "Not implemented"


storeProject :: ProjectManager -> Project.ID -> IO ()
storeProject = error "Not implemented"
