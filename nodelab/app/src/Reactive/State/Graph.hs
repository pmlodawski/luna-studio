{-# LANGUAGE OverloadedStrings #-}

module Reactive.State.Graph where

import           Utils.PreludePlus hiding ((.=))
import           Utils.Vector

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Text.Lazy   as Text
import           Debug.Trace

import           Object.Object
import           Object.Port
import           Object.Node

import           Data.Aeson
import           Utils.Aeson (intMapToJSON)


data Connection = Connection { _connId      :: ConnectionId
                             , _source      :: PortRef
                             , _destination :: PortRef
                             } deriving (Eq, Show, Generic)

makeLenses ''Connection
instance ToJSON Connection

type ConnectionsMap = IntMap Connection

data State = State { _nodesMap       :: NodesMap       -- don't access it directly
                   , _connectionsMap :: ConnectionsMap -- don't access it directly
                   } deriving (Show, Generic)

makeLenses ''State

instance ToJSON State where
    toJSON st = object [ "_nodes"       .= (intMapToJSON $ st ^. nodesMap)
                       , "_connections" .= (intMapToJSON $ st ^. connectionsMap)
                       ]

instance Eq State where
    a == b = (a ^. nodesMap) == (b ^. nodesMap)

instance Default State where
    def = State def def

connectionToRefs :: Connection -> (PortRef, PortRef)
connectionToRefs conn = (conn ^. source, conn ^. destination)

connectionToNodeIds :: Connection -> (NodeId, NodeId)
connectionToNodeIds conn = (src ^. refPortNodeId, dst ^. refPortNodeId) where
    (src, dst) = connectionToRefs conn

genId :: IntMap a -> ID
genId intMap = if IntMap.null intMap then 0
                                     else 1 + (fst $ IntMap.findMax intMap)

genNodeId :: State -> NodeId
genNodeId state = genId $ state ^. nodesMap

genConnectionId :: State -> NodeId
genConnectionId state = genId $ state ^. connectionsMap

getNode :: State -> NodeId -> Node
getNode state nodeId = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId $ state ^. nodesMap

getNodeById :: State -> NodeId -> Maybe Node
getNodeById state nodeId = IntMap.lookup nodeId $ state ^. nodesMap

nodes :: Getter State NodeCollection
nodes = to getNodes

getNodes :: State -> NodeCollection
getNodes = IntMap.elems . getNodesMap

getNodesMap :: State -> NodesMap
getNodesMap = (^. nodesMap)

getConnections :: State -> [Connection]
getConnections = IntMap.elems . getConnectionsMap

getConnectionsMap :: State -> ConnectionsMap
getConnectionsMap = (^. connectionsMap)

getConnectionNodeIds :: ConnectionId -> State -> Maybe (NodeId, NodeId)
getConnectionNodeIds connId state = (mapTup _refPortNodeId) <$> refs
    where mapTup f (a,b) = (f a, f b)
          conn = lookUpConnection state connId
          refs = connectionToRefs <$> conn

updateNodes :: NodesMap -> State -> State
updateNodes newNodesMap state = state & nodesMap .~ newNodesMap

getPort :: State -> PortRef -> Port
getPort state portRef = fromMaybe err $ find (\port -> port ^. portId == portRef ^. refPortId) ports where
    node  = getNode state $ portRef ^. refPortNodeId
    ports = getPorts (portRef ^. refPortType) node
    err   = error $ "Port " <> show (portRef ^. refPortId) <> " not found"

addNode :: Node -> State -> State
addNode newNode state  = state & nodesMap     %~ IntMap.insert (newNode ^. nodeId) newNode

removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodesMap %~ IntMap.delete remNodeId

addConnection :: PortRef -> PortRef -> State -> (Maybe ConnectionId, State) -- TODO: check if node/ports exist
addConnection sourcePortRef destPortRef state = (Just newConnId, newState) where
    newState      = state & connectionsMap %~ IntMap.insert (newConnection ^. connId) newConnection
    newConnection = Connection newConnId sourcePortRef destPortRef
    newConnId     = genConnectionId state

removeConnections :: [ConnectionId] -> State -> State
removeConnections connIds state = foldr removeConnection state connIds

removeConnection :: ConnectionId -> State -> State
removeConnection connId state = state & connectionsMap %~ IntMap.delete connId

lookUpConnection :: State -> ConnectionId -> Maybe Connection
lookUpConnection state connId = IntMap.lookup connId $ getConnectionsMap state

containsNode :: NodeId -> Connection -> Bool
containsNode id conn = (conn ^. source      . refPortNodeId == id)
                    || (conn ^. destination . refPortNodeId == id)

startsWithNode :: NodeId -> Connection -> Bool
startsWithNode id conn = conn ^. source . refPortNodeId == id

endsWithNode :: NodeId -> Connection -> Bool
endsWithNode id conn = conn ^. destination . refPortNodeId == id

connectionsContainingNode :: NodeId -> State -> [Connection]
connectionsContainingNode id state = filter (containsNode id) $ getConnections state

connectionIdsContainingNode :: NodeId -> State -> [ConnectionId]
connectionIdsContainingNode id state = (^. connId) <$> connectionsContainingNode id state

connectionsStartingWithNode :: NodeId -> State -> [Connection]
connectionsStartingWithNode id state = filter (startsWithNode id) $ getConnections state

connectionsEndingWithNode :: NodeId -> State -> [Connection]
connectionsEndingWithNode id state = filter (endsWithNode id) $ getConnections state

hasConnections :: NodeId -> State -> Bool
hasConnections nodeId state = not . null $ connectionsContainingNode nodeId state
