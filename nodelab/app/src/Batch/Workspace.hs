module Batch.Workspace where

import Utils.PreludePlus
import Batch.Breadcrumbs
import Data.Aeson (ToJSON)
import Data.IntMap.Lazy (IntMap)

import           Empire.API.Data.Project (Project, ProjectId)
import qualified Empire.API.Data.Project as Project
import           Empire.API.Data.Library (Library, LibraryId)
import qualified Empire.API.Data.Library as Library
import           Empire.API.JSONInstances ()

data InterpreterState = Fresh
                      | InsertsInProgress Int
                      | AllSet
                      deriving (Show, Eq, Generic)

data Workspace = Workspace { _projects         :: IntMap Project
                           , _currentProjectId :: ProjectId
                           , _currentLibraryId :: LibraryId
                           , _interpreterState :: InterpreterState
                           , _shouldLayout     :: Bool
                           } deriving (Show, Eq, Generic)

instance ToJSON InterpreterState
instance ToJSON Workspace

makeLenses ''Workspace

currentProject' :: Workspace -> Project
currentProject' w = fromMaybe err $ w ^? projects . ix id where
    id = w ^. currentProjectId
    err = error "Invalid project id"

currentProject :: Getter Workspace Project
currentProject = to currentProject'

currentLibrary' :: Workspace -> Library
currentLibrary' w = fromMaybe err $ project ^? Project.libs . ix id where
    id = w ^. currentLibraryId
    project = w ^. currentProject
    err = error "Invalid library id"

currentLibrary :: Getter Workspace Library
currentLibrary = to currentLibrary'

addSerializationMode :: Int -> InterpreterState -> InterpreterState
addSerializationMode goal current = case current of
    Fresh -> InsertsInProgress 1
    InsertsInProgress x | (x + 1) == goal -> AllSet
                        | otherwise       -> InsertsInProgress $ x + 1
    AllSet -> AllSet
