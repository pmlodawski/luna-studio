module Empire.API.Library.ListLibraries where

import Prologue
import Data.Binary                      (Binary)

import Empire.API.Data.Project          (ProjectId)

data ListLibraries = ListLibraries { _projectId   :: ProjectId
                                   } deriving (Generic, Show, Eq)

makeLenses ''ListLibraries

instance Binary ListLibraries



-- listLibraries :: ProjectId -> Empire [(LibraryId, Library)]
