module Empire.API.Graph.Disconnect where

import           Prologue
import           Data.Binary                (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node       (NodeId)
import           Empire.API.Data.Port       (OutPort, InPort)
import qualified Empire.API.Response        as Response

data Request = Request { _location   :: GraphLocation
                       , _dstNodeId  :: NodeId
                       , _dstPort    :: InPort
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request

instance Binary Request
