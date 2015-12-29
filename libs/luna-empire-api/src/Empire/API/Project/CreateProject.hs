module Empire.API.Project.CreateProject where

import Prologue
import Data.Binary                      (Binary)
import System.Path                      (Path)

import Empire.Binary.Instances.Missing

data CreateProject = CreateProject { _projectName :: Maybe String
                                   , _path        :: Path
                                   } deriving (Generic, Show, Eq)

makeLenses ''CreateProject

instance Binary CreateProject


-- createProject :: Maybe String -> Path -> Empire (ProjectId, Project)
