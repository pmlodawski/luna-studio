module Empire.API.Graph.NodeUpdate where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import qualified Empire.API.Topic              as T

data Update = Update { _location  :: GraphLocation
                     , _node      :: Node
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.node"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
