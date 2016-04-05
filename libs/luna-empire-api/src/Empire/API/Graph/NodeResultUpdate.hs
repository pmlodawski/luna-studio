module Empire.API.Graph.NodeResultUpdate where

import           Data.Binary                   (Binary)
import           Prologue

import qualified Empire.API.Data.DefaultValue  as DV (Value)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Error         (Error)

data NodeValue = NoValue | Value DV.Value | Error (Error String) deriving (Show, Eq, Generic)

data Update = Update { _location :: GraphLocation
                     , _nodeId   :: NodeId
                     , _value    :: NodeValue
                     , _execTime :: Integer
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
instance Binary NodeValue
