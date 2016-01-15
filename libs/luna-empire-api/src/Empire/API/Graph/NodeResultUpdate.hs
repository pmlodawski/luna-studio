module Empire.API.Graph.NodeResultUpdate where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)

data Update = Update { _location  :: GraphLocation
                     , _nodeId    :: NodeId
                     , _value     :: Int
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
