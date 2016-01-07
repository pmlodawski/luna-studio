module Empire.API.Library.CreateLibrary where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Response     as Response

data Request = Request { _projectId   :: ProjectId
                       , _libraryName :: Maybe String
                       , _path        :: String
                       } deriving (Generic, Show, Eq)

data Update = Update { _libraryId :: LibraryId
                     , _library   :: Library
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
