module Empire.API.Project.CreateProject where

import           Prologue
import           Data.Binary                      (Binary)

import           Empire.API.Data.Project          (ProjectId, Project)
import qualified Empire.API.Update              as Update

data Request = Request { _projectName :: Maybe String
                       , _path        :: String
                       } deriving (Generic, Show, Eq)

data Result = Result { _projectId :: ProjectId
                     , _project   :: Project
                     } deriving (Generic, Show, Eq)

type Update = Update.Update Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
