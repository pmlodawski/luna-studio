module Empire.API.Library.ListLibraries where

import Prologue
import Data.Binary                      (Binary)

import Empire.API.Data.Project          (ProjectId)
import Empire.API.Data.Library          (LibraryId)

data ListLibraries = ListLibraries { _projectId   :: ProjectId
                                   } deriving (Generic, Show, Eq)

data ListLibrariesResponse = ListLibrariesResponse { _libraryIds   :: [LibraryId]
                                                   -- , _libraries   :: [(LibraryId, Library)]
                                                   } deriving (Generic, Show, Eq)


makeLenses ''ListLibraries
makeLenses ''ListLibrariesResponse

instance Binary ListLibraries
instance Binary ListLibrariesResponse

