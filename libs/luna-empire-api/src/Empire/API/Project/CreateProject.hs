module Empire.API.Project.CreateProject where

import           Prologue
import           Data.Binary                      (Binary)

import           Empire.API.Data.Project          (ProjectId, Project)
import qualified Empire.API.Response              as Response

data Request = Request { _projectName :: Maybe String
                       , _path        :: String
                       } deriving (Generic, Show, Eq)

data Update = Update { _projectId :: ProjectId
                     , _project   :: Project
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
