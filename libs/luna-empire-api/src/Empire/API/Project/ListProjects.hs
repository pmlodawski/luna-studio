module Empire.API.Project.ListProjects where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import qualified Empire.API.Response     as Response

data Request = Request deriving (Generic, Show, Eq)

data Update = Update { _projectIds   :: [ProjectId]
                     -- , _projects   :: [(ProjectId, Project)]
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
