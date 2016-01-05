module Empire.API.Graph.AddNode where

import Prologue
import Data.Binary              (Binary)

import Empire.API.Data.Project  (ProjectId)
import Empire.API.Data.Library  (LibraryId)
import Empire.API.Data.Node     (Node)
import Empire.API.Data.NodeMeta (NodeMeta)
import Empire.API.Response

data AddNode = AddNode { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       , _nodeMeta  :: NodeMeta
                       , _tag       :: Int
                       } deriving (Generic, Show, Eq)

data AddNodeResult = AddNodeResult { _node :: Node
                                   -- , _breadcrumb :: BreadCrumb
                                   } deriving (Generic, Show, Eq)

type AddNodeResponse = Response AddNode AddNodeResult

makeLenses ''AddNode
makeLenses ''AddNodeResult

instance Binary AddNode
instance Binary AddNodeResult
