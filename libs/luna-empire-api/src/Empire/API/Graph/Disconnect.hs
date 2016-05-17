module Empire.API.Graph.Disconnect where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (InPort, OutPort)
import           Empire.API.Data.PortRef       (InPortRef (..))
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location :: GraphLocation
                       , _dst      :: InPortRef
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

data Update = Update   { _location' :: GraphLocation
                       , _dst'      :: InPortRef
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance Binary Update

instance G.GraphRequest Request where location = location