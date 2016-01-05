module Empire.API.Data.Connection where

import Prologue
import Empire.API.Data.Port (OutPort, InPort, PortId(..))
import Empire.API.Data.Node (NodeId)
import Data.Binary          (Binary)


type ConnectionId = Int

data InPortRef  = InPortRef  { _dstNodeId :: NodeId
                             , _dstPortId :: InPort
                             } deriving (Show, Eq, Generic, Ord)

data OutPortRef = OutPortRef { _srcNodeId :: NodeId
                             , _srcPortId :: OutPort
                             } deriving (Show, Eq, Generic, Ord)

data AnyPortRef = OutPortRef' OutPortRef | InPortRef' InPortRef deriving (Show, Eq, Generic)

instance Ord AnyPortRef where
  (InPortRef'  a) <= (OutPortRef' b) = True
  (OutPortRef' a) <= (InPortRef'  b) = False
  _ <= _ = True

nodeId' :: AnyPortRef -> NodeId
nodeId' (OutPortRef' (OutPortRef nid _)) = nid
nodeId' (InPortRef'  (InPortRef  nid _)) = nid

nodeId :: Getter AnyPortRef NodeId
nodeId = to nodeId'

portId' :: AnyPortRef -> PortId
portId' (OutPortRef' (OutPortRef _ pid)) = OutPortId pid
portId' (InPortRef'  (InPortRef  _ pid)) = InPortId  pid

portId :: Getter AnyPortRef PortId
portId = to portId'

toAnyPortRef :: NodeId -> PortId -> AnyPortRef
toAnyPortRef nid (InPortId pid)  = InPortRef'  $ InPortRef  nid pid
toAnyPortRef nid (OutPortId pid) = OutPortRef' $ OutPortRef nid pid


data Connection = Connection { _connectionId :: ConnectionId
                             , _src          :: OutPortRef
                             , _dst          :: InPortRef
                             } deriving (Show, Eq, Generic)

makeLenses ''Connection
makeLenses ''AnyPortRef
makeLenses ''OutPortRef
makeLenses ''InPortRef
