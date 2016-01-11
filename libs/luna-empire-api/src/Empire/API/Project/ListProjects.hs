module Empire.API.Project.ListProjects where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId, Project)
import qualified Empire.API.Response     as Response

data Request = Request deriving (Generic, Show, Eq)

data Status = Status { _projects   :: [(ProjectId, Project)]
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Status

makeLenses ''Request
makeLenses ''Status

instance Binary Request
instance Binary Status
