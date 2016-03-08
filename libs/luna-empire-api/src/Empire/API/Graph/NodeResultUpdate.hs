module Empire.API.Graph.NodeResultUpdate where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.DefaultValue  (Value)

data Update = Update { _location  :: GraphLocation
                     , _nodeId    :: NodeId
                     , _value     :: Maybe Value
                     , _execTime  :: Integer
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
