module Empire.API.Project.ListProjects where

import Prologue
import Data.Binary                      (Binary)

import Empire.API.Data.Project          (ProjectId)
import Empire.API.Response

data ListProjects = ListProjects deriving (Generic, Show, Eq)

data ListProjectsResult = ListProjectsResult { _projectIds   :: [ProjectId]
                                             -- , _projects   :: [(ProjectId, Project)]
                                             } deriving (Generic, Show, Eq)

data ListProjectsResponse = Response ListProjects ListProjectsResult

makeLenses ''ListProjectsResult

instance Binary ListProjects
instance Binary ListProjectsResult
