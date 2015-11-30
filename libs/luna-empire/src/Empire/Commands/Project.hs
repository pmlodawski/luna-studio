module Empire.Commands.Project where

import           Prologue
import           Control.Monad.State
import           Empire.Empire       (Empire, EmpireAction, ProjectManager)
import qualified Empire.Empire       as Empire
import           Empire.Data.Project (Project, ProjectId)
import qualified Empire.Data.Project as Project
import           System.Path         (Path)
import qualified Data.IntMap         as IntMap

insertAtNewId :: Project -> EmpireAction ProjectManager ProjectId
insertAtNewId project = do
    pm <- get
    let key = if IntMap.null pm then 0 else 1 + (fst . IntMap.findMax $ pm)
    at key ?= project
    return key

createProject :: Maybe String -> Path -> Empire (ProjectId, Project)
createProject name path = do
    let project = Project.make name path
    id <- zoom Empire.projectManager $ insertAtNewId project
    return (id, project)

listProjects :: Empire [(ProjectId, Project)]
listProjects = uses Empire.projectManager IntMap.toList
