module Empire.API.Graph.SetDefaultValue where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (InPort)
import           Empire.API.Data.DefaultValue  (PortDefault)
import qualified Empire.API.Update             as Update

data Request = Request { _location     :: GraphLocation
                       , _nodeId       :: NodeId
                       , _portId       :: InPort
                       , _defaultValue :: Maybe PortDefault
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary Request
