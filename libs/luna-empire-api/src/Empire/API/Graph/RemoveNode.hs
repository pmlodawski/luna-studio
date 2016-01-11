module Empire.API.Graph.RemoveNode where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.API.Response     as Response

data Request = Request { _location  :: GraphLocation
                       , _nodeId    :: NodeId
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request

instance Binary Request
