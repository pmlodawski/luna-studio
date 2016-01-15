module Empire.API.Data.Node where

import           Prologue
import           Data.Binary   (Binary)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.NodeMeta as NodeMeta
import           Empire.API.Data.Port     (PortId, Port)
import qualified Empire.API.Data.Port     as Port

type NodeId = Int

data Node = Node { _nodeId      :: NodeId
                 , _expression  :: Text
                 , _ports       :: Map PortId Port
                 , _nodeMeta    :: NodeMeta
                 } deriving (Generic, Typeable, Show, Eq)

makeLenses ''Node

mockPorts :: Map PortId Port
mockPorts = Map.fromList [ (Port.InPortId Port.Self,    Port.Port (Port.InPortId Port.Self)    (Port.ValueType "Int") Nothing)
                         , (Port.InPortId (Port.Arg 0), Port.Port (Port.InPortId (Port.Arg 0)) (Port.ValueType "Int") Nothing)
                         , (Port.OutPortId Port.All,    Port.Port (Port.OutPortId Port.All)    (Port.ValueType "Int") Nothing)
                         ]

make :: NodeId -> Text -> NodeMeta -> Node
make id expr meta = Node id expr mockPorts meta

position :: Lens' Node (Double, Double)
position = nodeMeta . NodeMeta.position

instance Binary Node
