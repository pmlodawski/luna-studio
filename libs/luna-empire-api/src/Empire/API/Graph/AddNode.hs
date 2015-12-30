module Empire.API.Graph.AddNode where

import Prologue
import Data.Binary             (Binary)

import Empire.API.Data.Project (ProjectId)
import Empire.API.Data.Library (LibraryId)
import Empire.API.Data.Node    (NodeId)

data AddNode = AddNode { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       } deriving (Generic, Show, Eq)

data AddNodeResponse = AddNodeResponse { _nodeId :: NodeId
                                       } deriving (Generic, Show, Eq)

makeLenses ''AddNode
makeLenses ''AddNodeResponse

instance Binary AddNode
instance Binary AddNodeResponse
