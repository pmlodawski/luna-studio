module Empire.API.Project.CreateProject where

import Prologue
import Data.Binary                      (Binary)
import System.Path                      (Path)

import Empire.API.Data.Project          (ProjectId)
import Empire.API.Response

import Empire.Binary.Instances.Missing

data CreateProject = CreateProject { _projectName :: Maybe String
                                   , _path        :: Path
                                   } deriving (Generic, Show, Eq)

data CreateProjectResult = CreateProjectResult { _projectId :: ProjectId
                                               -- , _project   :: Project
                                               } deriving (Generic, Show, Eq)

type CreateProjectResponse = Response CreateProject CreateProjectResult

makeLenses ''CreateProject
makeLenses ''CreateProjectResult

instance Binary CreateProject
instance Binary CreateProjectResult
