module Empire.API.Graph.Disconnect where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (OutPort, InPort)
import           Empire.API.Data.PortRef       (InPortRef(..))
import qualified Empire.API.Update             as Update

data Request = Request { _location  :: GraphLocation
                       , _dstNodeId :: NodeId
                       , _dstPort   :: InPort
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary Request

dst' :: Request -> InPortRef
dst' r = InPortRef (r ^. dstNodeId) (r ^. dstPort)

dst :: Getter Request InPortRef
dst = to dst'
