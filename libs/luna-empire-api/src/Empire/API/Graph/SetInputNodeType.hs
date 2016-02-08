module Empire.API.Graph.SetInputNodeType where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Update             as Update

data Request = Request { _location :: GraphLocation
                       , _nodeId   :: NodeId
                       , _tpe      :: String
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary Request
