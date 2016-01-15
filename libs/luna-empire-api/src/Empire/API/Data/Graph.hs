module Empire.API.Data.Graph where

import Prologue
import Data.Binary (Binary)

import Empire.API.Data.Node       (Node)
import Empire.API.Data.PortRef    (OutPortRef, InPortRef)

data Graph = Graph { _nodes       :: [Node]
                   , _connections :: [(OutPortRef, InPortRef)]
                   } deriving (Show, Eq, Generic)

makeLenses ''Graph
instance Binary Graph
