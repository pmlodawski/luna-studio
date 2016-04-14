module Empire.API.Graph.RemoveNode where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Update             as Update

data Request = Request { _location :: GraphLocation
                       , _nodeIds  :: [NodeId]
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary Request
