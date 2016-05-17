module Empire.API.Graph.SetDefaultValue where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (AnyPortRef)
import           Empire.API.Data.DefaultValue  (PortDefault)
import qualified Empire.API.Response            as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location     :: GraphLocation
                       , _portRef      :: AnyPortRef
                       , _defaultValue :: PortDefault
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location