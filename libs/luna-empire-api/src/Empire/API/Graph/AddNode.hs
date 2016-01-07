module Empire.API.Graph.AddNode where

import           Prologue
import           Data.Binary              (Binary)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Node     (Node)
import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Response      as Response

data Request = Request { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       , _nodeMeta  :: NodeMeta
                       , _tag       :: Int
                       } deriving (Generic, Show, Eq)

data Update = Update { _node    :: Node
                     -- , _breadcrumb :: BreadCrumb
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
