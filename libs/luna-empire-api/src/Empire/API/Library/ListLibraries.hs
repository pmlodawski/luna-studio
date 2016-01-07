module Empire.API.Library.ListLibraries where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId)
import qualified Empire.API.Response     as Response

data Request = Request { _projectId   :: ProjectId
                       } deriving (Generic, Show, Eq)

data Update = Update { _libraryIds   :: [LibraryId]
                     -- , _libraries   :: [(LibraryId, Library)]
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update

