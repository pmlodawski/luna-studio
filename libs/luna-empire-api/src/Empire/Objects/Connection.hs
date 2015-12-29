module Empire.Objects.Connection where

import Prologue
import Empire.Objects.Port (OutPort, InPort)
import Empire.Objects.Node (NodeId)

data InPortRef  = InPortRef  { _dstNodeId :: NodeId
                             , _dstPortId :: InPort
                             } deriving (Show, Eq)

data OutPortRef = OutPortRef { _srcNodeId :: NodeId
                             , _srcPortId :: OutPort
                             } deriving (Show, Eq)

data Connection = Connection { _src :: OutPortRef
                             , _dst :: InPortRef
                             } deriving (Show, Eq)

makeLenses ''Connection
