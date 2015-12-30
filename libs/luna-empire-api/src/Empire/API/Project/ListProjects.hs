module Empire.API.Project.ListProjects where

import Prologue
import Data.Binary                      (Binary)

import Empire.API.Data.Project          (ProjectId)

data ListProjects = ListProjects deriving (Generic, Show, Eq)

data ListProjectsResponse = ListProjectsResponse { _projectIds   :: [ProjectId]
                                                 -- , _projects   :: [(ProjectId, Project)]
                                                 } deriving (Generic, Show, Eq)

makeLenses ''ListProjectsResponse

instance Binary ListProjects
instance Binary ListProjectsResponse
