{-# LANGUAGE Rank2Types #-}
module Empire.API.Data.Connection where

import Prologue
import Empire.API.Data.PortRef (InPortRef, OutPortRef)
import Data.Binary             (Binary)


-- FIXME: Najpewniej to wyladuje calkowicie w GUI

type ConnectionId = InPortRef
data Connection = Connection { _src          :: OutPortRef
                             , _dst          :: InPortRef
                             } deriving (Show, Eq, Generic)

makeLenses ''Connection
instance Binary Connection

connectionId :: Lens' Connection ConnectionId
connectionId = dst

