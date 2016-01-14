module Empire.API.Graph.UpdateNodeMeta where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Update             as Update

data Request = Request { _location :: GraphLocation
                       , _nodeId   :: NodeId
                       , _nodeMeta :: NodeMeta
                       } deriving (Generic, Show, Eq)

data Result = Result { _newNodeMeta :: NodeMeta
                     } deriving (Generic, Show, Eq)

type Update = Update.Update Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
