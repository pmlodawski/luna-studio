module Empire.API.Library.ListLibraries where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Response     as Response

data Request = Request { _projectId   :: ProjectId
                       } deriving (Generic, Show, Eq)

data Status = Status { _libraries   :: [(LibraryId, Library)]
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Status

makeLenses ''Request
makeLenses ''Status

instance Binary Request
instance Binary Update

