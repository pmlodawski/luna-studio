module Empire.API.Graph.AddNode where

import Prologue
import Empire.Objects.Project (ProjectId)
import Empire.Objects.Library (LibraryId)

data AddNode = AddNode { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       } deriving (Show, Eq)

makeLenses ''AddNode
