---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Project.ProjectManager (
    module Flowbox.Data.Graph,
    module Flowbox.Batch.Project.ProjectManager,
) where

import           Flowbox.Batch.Project.Project   (Project)
import qualified Flowbox.Batch.Project.Project   as Project
import qualified Flowbox.Batch.Project.Serialize as Serialize
import           Flowbox.Data.Graph              hiding (Edge, delNode, insNewNode, lab, labNodes, nodes, updateNode)
import qualified Flowbox.Data.Graph              as Graph
import           Flowbox.Prelude
import           Flowbox.System.UniPath          (UniPath)



type ProjectManager = Graph Project ()


openProject :: ProjectManager -> UniPath -> IO (ProjectManager, (Project.ID, Project))
openProject projectManager ppath = do
    project <- Serialize.restoreProject ppath
    let (newProjectManager, projectID) = insNewNode project projectManager
    return (newProjectManager, (projectID, project))


lab :: ProjectManager -> Project.ID -> Maybe Project
lab pm = Graph.lab pm . Project.toInt


labNodes :: ProjectManager -> [(Project.ID, Project)]
labNodes = over (mapped . _1) Project.ID . Graph.labNodes


nodes :: ProjectManager -> [Project.ID]
nodes = map Project.ID . Graph.nodes


updateNode :: (Project.ID, Project) -> ProjectManager -> ProjectManager
updateNode project = Graph.updateNode (_1 %~ Project.toInt $ project)


insNewNode :: Project -> ProjectManager -> (ProjectManager, Project.ID)
insNewNode project pm = _2 %~ Project.ID $ Graph.insNewNode project pm


delNode :: Project.ID -> ProjectManager -> ProjectManager
delNode projectID = Graph.delNode $ Project.toInt projectID
