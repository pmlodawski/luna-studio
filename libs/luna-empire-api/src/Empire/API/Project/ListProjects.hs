module Empire.API.Project.ListProjects where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId, Project)
import qualified Empire.API.Response             as Response

data Request = Request deriving (Generic, Show, Eq)

data Result  = Result { _projects :: [(ProjectId, Project)]
                      } deriving (Generic, Show, Eq)

type Response = Response.Response Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
