module Empire.API.Graph.Collaboration where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R
import           Data.UUID.Types (UUID)

type ClientId = UUID

data Event = Modify      [NodeId]
           | Touch       [NodeId]
           | CancelTouch [NodeId]
           deriving (Generic, Show, Eq)

data Update   = Update { _location  :: GraphLocation
                       , _clientId  :: ClientId
                       , _event     :: Event
                       } deriving (Generic, Show, Eq)


makeLenses ''Update
makeLenses ''Event

instance Binary Update
instance Binary Event

topicPrefix = "empire.graph.collaboration"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
