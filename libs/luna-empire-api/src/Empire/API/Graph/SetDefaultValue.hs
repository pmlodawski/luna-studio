module Empire.API.Graph.SetDefaultValue where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (AnyPortRef)
import           Empire.API.Data.DefaultValue  (PortDefault)
import qualified Empire.API.Update             as Update

data Request = Request { _location     :: GraphLocation
                       , _portRef      :: AnyPortRef
                       , _defaultValue :: PortDefault
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary Request
