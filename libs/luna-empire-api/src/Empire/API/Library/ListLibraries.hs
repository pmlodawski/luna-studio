module Empire.API.Library.ListLibraries where

import Prologue
import Data.Binary                      (Binary)

import Empire.API.Data.Project          (ProjectId)
import Empire.API.Data.Library          (LibraryId)
import Empire.API.Response

data ListLibraries = ListLibraries { _projectId   :: ProjectId
                                   } deriving (Generic, Show, Eq)

data ListLibrariesResult = ListLibrariesResult { _libraryIds   :: [LibraryId]
                                               -- , _libraries   :: [(LibraryId, Library)]
                                               } deriving (Generic, Show, Eq)

data ListLibrariesResponse = Response ListLibraries ListLibrariesResult

makeLenses ''ListLibraries
makeLenses ''ListLibrariesResult

instance Binary ListLibraries
instance Binary ListLibrariesResult

