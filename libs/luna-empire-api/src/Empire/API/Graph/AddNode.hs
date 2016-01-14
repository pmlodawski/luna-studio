module Empire.API.Graph.AddNode where

import           Prologue
import           Data.Binary                   (Binary)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Update             as Update

data Request = Request { _location :: GraphLocation
                       , _expr     :: String
                       , _nodeMeta :: NodeMeta
                       , _tag      :: Int
                       } deriving (Generic, Show, Eq)

data Result = Result { _node :: Node
                     } deriving (Generic, Show, Eq)

type Update = Update.Update Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
