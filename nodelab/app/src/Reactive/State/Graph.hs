{-# LANGUAGE OverloadedStrings #-}

module Reactive.State.Graph where

import           Utils.PreludePlus hiding ((.=))
import           Utils.Vector

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Text.Lazy   as Text
import           Debug.Trace

import           Data.Aeson
import           Utils.Aeson (intMapToJSON)
import           Empire.API.Data.Connection (Connection(..), ConnectionId)
import           Empire.API.Data.PortRef    (OutPortRef, InPortRef, AnyPortRef)
import qualified Empire.API.Data.Connection as Connection
import qualified Empire.API.Data.PortRef    as PortRef
import           Empire.API.Data.Node       (Node, NodeId)
import qualified Empire.API.Data.Node       as Node
import           Empire.API.Data.Port       (Port)
import qualified Empire.API.Data.Port       as Port
import qualified Empire.API.JSONInstances ()

type NodesMap       = IntMap Node
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

connectionToNodeIds :: Connection -> (NodeId, NodeId)
connectionToNodeIds conn = ( conn ^. Connection.src . PortRef.srcNodeId
                           , conn ^. Connection.dst . PortRef.dstNodeId)

genId :: IntMap a -> Int
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

nodes :: Getter State [Node]
nodes = to getNodes

getNodes :: State -> [Node]
getNodes = IntMap.elems . getNodesMap

getNodesMap :: State -> NodesMap
getNodesMap = (^. nodesMap)

getConnections :: State -> [Connection]
getConnections = IntMap.elems . getConnectionsMap

getConnectionsMap :: State -> ConnectionsMap
getConnectionsMap = (^. connectionsMap)

getConnectionNodeIds :: ConnectionId -> State -> Maybe (NodeId, NodeId)
getConnectionNodeIds connId state = connectionToNodeIds <$> conn
    where conn = lookUpConnection state connId

updateNodes :: NodesMap -> State -> State
updateNodes newNodesMap state = state & nodesMap .~ newNodesMap

getPort :: State -> AnyPortRef -> Port
getPort state portRef = fromMaybe err $ node ^? Node.ports . ix (portRef ^. PortRef.portId) where
    node  = getNode state $ portRef ^. PortRef.nodeId
    err   = error $ "Port " <> show portRef <> " not found"

addNode :: Node -> State -> State
addNode newNode state  = state & nodesMap     %~ IntMap.insert (newNode ^. Node.nodeId) newNode

removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodesMap %~ IntMap.delete remNodeId

addConnection :: OutPortRef -> InPortRef -> State -> (Maybe ConnectionId, State) -- TODO: check if node/ports exist
addConnection sourcePortRef destPortRef state = (Just newConnId, newState) where
    newState      = state & connectionsMap %~ IntMap.insert (newConnection ^. Connection.connectionId) newConnection
    newConnection = Connection newConnId sourcePortRef destPortRef
    newConnId     = genConnectionId state

removeConnections :: [ConnectionId] -> State -> State
removeConnections connIds state = foldr removeConnection state connIds

removeConnection :: ConnectionId -> State -> State
removeConnection connId state = state & connectionsMap %~ IntMap.delete connId

lookUpConnection :: State -> ConnectionId -> Maybe Connection
lookUpConnection state connId = IntMap.lookup connId $ getConnectionsMap state

containsNode :: NodeId -> Connection -> Bool
containsNode id conn = (startsWithNode id conn)
                    || (endsWithNode id conn)

startsWithNode :: NodeId -> Connection -> Bool
startsWithNode id conn = conn ^. Connection.src . PortRef.srcNodeId == id

endsWithNode :: NodeId -> Connection -> Bool
endsWithNode id conn = conn ^. Connection.dst . PortRef.dstNodeId == id

connectionsContainingNode :: NodeId -> State -> [Connection]
connectionsContainingNode id state = filter (containsNode id) $ getConnections state

connectionIdsContainingNode :: NodeId -> State -> [ConnectionId]
connectionIdsContainingNode id state = (^. Connection.connectionId) <$> connectionsContainingNode id state

connectionsStartingWithNode :: NodeId -> State -> [Connection]
connectionsStartingWithNode id state = filter (startsWithNode id) $ getConnections state

connectionsEndingWithNode :: NodeId -> State -> [Connection]
connectionsEndingWithNode id state = filter (endsWithNode id) $ getConnections state

hasConnections :: NodeId -> State -> Bool
hasConnections nodeId state = not . null $ connectionsContainingNode nodeId state
