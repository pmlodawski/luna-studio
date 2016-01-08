module Empire.API.Graph.AddNode where

import Prologue
import Empire.API.Data.Project (ProjectId)
import Empire.API.Data.Library (LibraryId)

data AddNode = AddNode { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       } deriving (Show, Eq)

makeLenses ''AddNode
