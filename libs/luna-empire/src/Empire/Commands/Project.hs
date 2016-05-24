module Empire.Commands.Project
    ( withProject
    , listProjects
    , createProject
    ) where

import           Control.Monad.Error     (throwError)
import           Control.Monad.State
import           Control.Monad.Reader
import           Prologue

import           Empire.API.Data.Project (ProjectId)
import           Empire.Data.Project     (Project)
import qualified Empire.Data.Project     as Project
import           Empire.Empire           (Command, Empire, ProjectManager)
import qualified Empire.Empire           as Empire
import qualified Empire.Utils.IdGen      as IdGen

import qualified Data.IntMap             as IntMap
import           Data.Map.Lazy           (Map)
import qualified Data.Map.Lazy           as Map

import           System.Path             (Path)

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
    notifEnv   <- ask
    case projectMay of
        Nothing      -> throwError $ "Project " ++ (show pid) ++ " does not exist."
        Just project -> do
            let result = (_2 %~ Just) <$> Empire.runEmpire notifEnv project cmd
            Empire.empire $ const $ const result

-- internal

insertAtNewId :: Project -> Command ProjectManager ProjectId
insertAtNewId project = do
    pm <- get
    let key = IdGen.nextId pm
    at key ?= project
    return key
