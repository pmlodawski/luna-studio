module Empire.API.Library.CreateLibrary where

import Prologue
import Data.Binary                      (Binary)
import System.Path                      (Path)

import Empire.API.Data.Project          (ProjectId)
import Empire.API.Data.Library          (LibraryId)
import Empire.API.Response

import Empire.Binary.Instances.Missing

data CreateLibrary = CreateLibrary { _projectId   :: ProjectId
                                   , _libraryName :: Maybe String
                                   , _path        :: Path
                                   } deriving (Generic, Show, Eq)

data CreateLibraryResult = CreateLibraryResult { _libraryId :: LibraryId
                                               -- , _library   :: Library
                                               } deriving (Generic, Show, Eq)

type CreateLibraryResponse = Response CreateLibrary CreateLibraryResult

makeLenses ''CreateLibrary
makeLenses ''CreateLibraryResult

instance Binary CreateLibrary
instance Binary CreateLibraryResult
