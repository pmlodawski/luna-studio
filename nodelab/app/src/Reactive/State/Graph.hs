{-# LANGUAGE OverloadedStrings #-}

module Reactive.State.Graph
    ( State(..)
    , addConnection
    , addNode
    , connectionIdsContainingNode
    , connectionWidgets
    , connectionWidgetsMap
    , connectionsContainingNode
    , connectionsMap
    , getConnectionNodeIds
    , getConnections
    , getConnectionsMap
    , getNodesMap
    , hasConnections
    , lookUpConnection
    , nodeWidgets
    , nodeWidgetsMap
    , nodes
    , nodesMap
    , portWidgets
    , portWidgetsMap
    , removeConnections
    , removeNode
    , updateNodes
    ) where

import           Utils.PreludePlus          hiding ((.=))

import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map.Strict            as Map
import           Data.UUID.Types            (UUID)

import           Data.Aeson
import           Empire.API.Data.Connection (Connection (..), ConnectionId)
import qualified Empire.API.Data.Connection as Connection
import           Empire.API.Data.Node       (Node, NodeId)
import qualified Empire.API.Data.Node       as Node
import           Empire.API.Data.Port       (InPort, OutPort)
import           Empire.API.Data.PortRef    (AnyPortRef, InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef    as PortRef
import qualified Empire.API.JSONInstances   ()
import           Object.UITypes             (WidgetId)
import           Reactive.Commands.Command  (Command)

type NodesMap       = HashMap NodeId Node
type ConnectionsMap = HashMap InPortRef Connection


instance (ToJSON b) => ToJSON (HashMap UUID b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

instance (ToJSON b) => ToJSON  (HashMap AnyPortRef b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

instance (ToJSON b) => ToJSON  (HashMap InPortRef b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

instance Default (HashMap a b) where def = HashMap.empty
instance Hashable InPort
instance Hashable OutPort
instance Hashable InPortRef
instance Hashable OutPortRef
instance Hashable AnyPortRef

data State = State { _nodesMap             :: NodesMap
                   , _connectionsMap       :: ConnectionsMap
                   , _nodeWidgetsMap       :: HashMap NodeId     WidgetId
                   , _connectionWidgetsMap :: HashMap InPortRef  WidgetId
                   , _portWidgetsMap       :: HashMap AnyPortRef WidgetId
                   } deriving (Show, Eq, Generic)

makeLenses ''State
instance ToJSON State
instance Default State where
    def = State def def def def def

connectionToNodeIds :: Connection -> (NodeId, NodeId)
connectionToNodeIds conn = ( conn ^. Connection.src . PortRef.srcNodeId
                           , conn ^. Connection.dst . PortRef.dstNodeId)

nodes :: Getter State [Node]
nodes = to getNodes

nodeWidgets :: Getter State [WidgetId]
nodeWidgets = to $ HashMap.elems . (view nodeWidgetsMap)

connectionWidgets :: Getter State [WidgetId]
connectionWidgets = to $ HashMap.elems . (view connectionWidgetsMap)

portWidgets :: Getter State [WidgetId]
portWidgets = to $ HashMap.elems . (view portWidgetsMap)

getNodes :: State -> [Node]
getNodes = HashMap.elems . getNodesMap

getNodesMap :: State -> NodesMap
getNodesMap = (^. nodesMap)

getConnections :: State -> [Connection]
getConnections = HashMap.elems . getConnectionsMap

getConnectionsMap :: State -> ConnectionsMap
getConnectionsMap = (^. connectionsMap)

getConnectionNodeIds :: ConnectionId -> State -> Maybe (NodeId, NodeId)
getConnectionNodeIds connId state = connectionToNodeIds <$> conn
    where conn = lookUpConnection state connId

updateNodes :: NodesMap -> State -> State
updateNodes newNodesMap state = state & nodesMap .~ newNodesMap

addNode :: Node -> State -> State
addNode newNode state  = state & nodesMap . at (newNode ^. Node.nodeId) ?~ newNode

removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodesMap . at remNodeId .~ Nothing

addConnection :: OutPortRef -> InPortRef -> Command State ConnectionId
addConnection sourcePortRef destPortRef = do
    connectionsMap . at destPortRef ?= Connection sourcePortRef destPortRef
    return destPortRef

removeConnections :: [ConnectionId] -> State -> State
removeConnections connIds state = foldr removeConnection state connIds

removeConnection :: ConnectionId -> State -> State
removeConnection connId state = state & connectionsMap . at connId .~ Nothing

lookUpConnection :: State -> ConnectionId -> Maybe Connection
lookUpConnection state connId = HashMap.lookup connId $ getConnectionsMap state

containsNode :: NodeId -> Connection -> Bool
containsNode id conn = (startsWithNode id conn)
                    || (endsWithNode   id conn)

startsWithNode :: NodeId -> Connection -> Bool
startsWithNode id conn = conn ^. Connection.src . PortRef.srcNodeId == id

endsWithNode :: NodeId -> Connection -> Bool
endsWithNode id conn = conn ^. Connection.dst . PortRef.dstNodeId == id

connectionsContainingNode :: NodeId -> State -> [Connection]
connectionsContainingNode id state = filter (containsNode id) $ getConnections state

connectionIdsContainingNode :: NodeId -> State -> [ConnectionId]
connectionIdsContainingNode id state = (^. Connection.connectionId) <$> connectionsContainingNode id state

hasConnections :: NodeId -> State -> Bool
hasConnections nodeId state = not . null $ connectionsContainingNode nodeId state
