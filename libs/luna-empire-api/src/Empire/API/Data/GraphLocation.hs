module Empire.API.Data.GraphLocation where

import           Prologue
import           Data.Binary                (Binary)

import           Empire.API.Data.Project    (ProjectId)
import           Empire.API.Data.Library    (LibraryId)
import           Empire.API.Data.Breadcrumb (Breadcrumb)

data GraphLocation = GraphLocation { _projectId  :: ProjectId
                                   , _libraryId  :: LibraryId
                                   , _breadcrumb :: Breadcrumb
                                   } deriving (Show, Eq, Generic)

makeLenses ''GraphLocation

instance Binary GraphLocation
