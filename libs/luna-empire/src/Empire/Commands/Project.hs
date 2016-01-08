module Empire.Commands.Project
    ( withProject
    , listProjects
    , createProject
    ) where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error     (throwError)

import           Empire.Empire           (Empire, Command, ProjectManager)
import qualified Empire.Empire           as Empire
import           Empire.Data.Project     (Project)
import qualified Empire.Data.Project     as Project
import           Empire.API.Data.Project (ProjectId)
import qualified Empire.Utils.IdGen     as IdGen

import           System.Path             (Path)
import qualified Data.IntMap             as IntMap

createProject :: Maybe String -> Path -> Empire (ProjectId, Project)
createProject name path = do
    let project = Project.make name path
    id <- zoom Empire.projectManager $ insertAtNewId project
    return (id, project)

listProjects :: Empire [(ProjectId, Project)]
listProjects = uses Empire.projectManager IntMap.toList

withProject :: ProjectId -> Command Project a -> Empire a
withProject pid cmd = zoom (Empire.projectManager . at pid) $ do
    projectMay <- get
    case projectMay of
        Nothing      -> throwError $ "Project " ++ (show pid) ++ " does not exist."
        Just project -> do
            let result = (_2 %~ Just) <$> Empire.runEmpire project cmd
            Empire.empire $ const result

-- internal

insertAtNewId :: Project -> Command ProjectManager ProjectId
insertAtNewId project = do
    pm <- get
    let key = IdGen.nextId pm
    at key ?= project
    return key
