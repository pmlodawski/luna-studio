module Empire.API.Graph.Disconnect where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.Port          (OutPort, InPort)
import           Empire.API.Data.PortRef       (InPortRef(..))
import qualified Empire.API.Update             as Update

data Request = Request { _location  :: GraphLocation
                       , _dstNodeId :: NodeId
                       , _dstPort   :: InPort
                       } deriving (Generic, Show, Eq)

data Result = Result { _node :: Node
                     } deriving (Generic, Show, Eq)

type Update = Update.Update Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result

dst' :: Request -> InPortRef
dst' r = InPortRef (r ^. dstNodeId) (r ^. dstPort)

dst :: Getter Request InPortRef
dst = to dst'
