module Empire.API.Data.Connection where

import Prologue
import Empire.API.Data.PortRef (InPortRef, OutPortRef)
import Data.Binary             (Binary)


-- FIXME: Najpewniej to wyladuje calkowicie w GUI

type ConnectionId = Int
data Connection = Connection { _connectionId :: ConnectionId
                             , _src          :: OutPortRef
                             , _dst          :: InPortRef
                             } deriving (Show, Eq, Generic)

makeLenses ''Connection
instance Binary Connection
