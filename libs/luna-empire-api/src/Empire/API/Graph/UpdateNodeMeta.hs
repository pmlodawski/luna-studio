module Empire.API.Graph.UpdateNodeMeta where

import           Prologue
import           Data.Binary              (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Response      as Response

data Request = Request { _location  :: GraphLocation
                       , _nodeId    :: NodeId
                       , _nodeMeta  :: NodeMeta
                       } deriving (Generic, Show, Eq)

data Update = Update { _newNodeMeta :: NodeMeta
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
