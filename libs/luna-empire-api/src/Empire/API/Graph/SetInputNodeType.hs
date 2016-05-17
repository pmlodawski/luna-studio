module Empire.API.Graph.SetInputNodeType where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Response             as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location :: GraphLocation
                       , _nodeId   :: NodeId
                       , _tpe      :: String
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request

instance Binary Request
instance G.GraphRequest Request where location = location