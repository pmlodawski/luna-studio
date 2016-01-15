module Empire.API.Data.Node where

import           Prologue
import           Data.Binary   (Binary)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import           Empire.API.Data.NodeMeta  (NodeMeta)
import qualified Empire.API.Data.NodeMeta  as NodeMeta
import           Empire.API.Data.ValueType (ValueType(..))
import           Empire.API.Data.Port      (PortId, Port)
import qualified Empire.API.Data.Port      as Port

type NodeId = Int

data Node = Node { _nodeId      :: NodeId
                 , _expression  :: Text
                 , _ports       :: Map PortId Port
                 , _nodeMeta    :: NodeMeta
                 } deriving (Generic, Typeable, Show, Eq)

makeLenses ''Node

position :: Lens' Node (Double, Double)
position = nodeMeta . NodeMeta.position

instance Binary Node
