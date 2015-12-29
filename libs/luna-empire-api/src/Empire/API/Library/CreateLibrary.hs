module Empire.API.Library.CreateLibrary where

import Prologue
import Data.Binary                      (Binary)
import System.Path                      (Path)

import Empire.API.Data.Project          (ProjectId)
import Empire.Binary.Instances.Missing

data CreateLibrary = CreateLibrary { _projectId   :: ProjectId
                                   , _libraryName :: Maybe String
                                   , _path        :: Path
                                   } deriving (Generic, Show, Eq)

makeLenses ''CreateLibrary

instance Binary CreateLibrary


-- createLibrary :: ProjectId -> Maybe String -> Path -> Empire (LibraryId, Library)

