module Empire.Data.Node where

import Prologue
import Data.Map.Lazy (Map)

import Empire.Data.Port (PortId, Port)

type NodeId = Int

data Node = Node { _nodeId      :: NodeId
                 , _position    :: (Double, Double) -- Use Vector2?
                 , _expression  :: Text
                 , _ports       :: Map PortId Port
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Node
