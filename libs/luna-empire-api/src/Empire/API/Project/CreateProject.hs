module Empire.API.Project.CreateProject where

import Prologue
import Data.Binary                      (Binary)
import System.Path                      (Path)

import Empire.API.Data.Project          (ProjectId)

import Empire.Binary.Instances.Missing

data CreateProject = CreateProject { _projectName :: Maybe String
                                   , _path        :: Path
                                   } deriving (Generic, Show, Eq)

data CreateProjectResponse = CreateProjectResponse { _projectId :: ProjectId
                                                   -- , _project   :: Project
                                                   } deriving (Generic, Show, Eq)

makeLenses ''CreateProject
makeLenses ''CreateProjectResponse

instance Binary CreateProject
instance Binary CreateProjectResponse
