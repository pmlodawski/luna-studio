module Empire.API.Graph.UpdateNodeMeta where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location :: GraphLocation
                       , _nodeId   :: NodeId
                       , _nodeMeta :: NodeMeta
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

data Update   = Update { _location' :: GraphLocation
                       , _nodeId'   :: NodeId
                       , _nodeMeta' :: NodeMeta
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
instance G.GraphRequest Request where location = location
