module Empire.API.Data.Graph where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Node    (Node)
import           Empire.API.Data.PortRef (InPortRef, OutPortRef)



data Graph = Graph { _nodes       :: [Node]
                   , _connections :: [(OutPortRef, InPortRef)]
                   } deriving (Show, Eq, Generic)

makeLenses ''Graph
instance Binary Graph
