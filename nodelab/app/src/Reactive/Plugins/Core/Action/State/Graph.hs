module Reactive.Plugins.Core.Action.State.Graph where


import           Utils.PreludePlus
import           Utils.Vector

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Text.Lazy   as Text
import           Debug.Trace

import           Object.Object
import           Object.Port
import           Object.Node

import           Luna.Syntax.Builder.Graph hiding (get, put)
import           Luna.Syntax.Builder
import           AST.AST


type NodesRefsMap = IntMap GraphRefMeta

data Connection = Connection { _connId      :: ConnectionId
                             , _source      :: PortRef
                             , _destination :: PortRef
                             } deriving (Eq, Show)

makeLenses ''Connection

type ConnectionsMap = IntMap Connection

data State = State { _nodesMap       :: NodesMap       -- don't access it directly
                   , _connectionsMap :: ConnectionsMap -- don't access it directly
                   , _nodesRefsMap   :: NodesRefsMap
                   , _focusedNodeId  :: NodeId
                   , _graphMeta      :: GraphMeta
                   } deriving (Show)

makeLenses ''State



-- TODO: Implement in full
instance Eq State where
    a == b = (a ^. nodesMap) == (b ^. nodesMap)

instance Default State where
    def = State def def def def def

instance PrettyPrinter Connection where
    display (Connection connId source destination) =
          "conn("  <> display connId
        <> " "     <> display source
        <> " "     <> display destination
        <> ")"

instance PrettyPrinter State where
    display (State nodesMap connections nodesRefsMap focusedNodeId bldrState) =
          "graph(" <> show nodesMap
        <> " "     <> display connections
        <> " "     <> display nodesRefsMap
        <> " "     <> display focusedNodeId
        <> " "     <> show bldrState
        <> ")"


genId :: IntMap a -> ID
genId intMap = if IntMap.null intMap then 0
                                     else 1 + (fst $ IntMap.findMax intMap)


genNodeId :: State -> NodeId
genNodeId state = genId $ state ^. nodesRefsMap

genConnectionId :: State -> NodeId
genConnectionId state = genId $ state ^. connectionsMap

getNode :: State -> NodeId -> Node
getNode state nodeId = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId $ state ^. nodesMap

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

updateNodes :: NodesMap -> State -> State
updateNodes newNodesMap state = state & nodesMap .~ newNodesMap


addNode :: Node -> State -> State
addNode newNode state  = state & nodesMap     %~ IntMap.insert (newNode ^. nodeId) newNode
                               & graphMeta    .~ newGraphMeta
                               & nodesRefsMap %~ IntMap.insert (newNode ^. nodeId) ref
    where (ref, newGraphMeta) = makeVar newNode $ rebuild $ state ^. graphMeta


-- TODO: nodeRefs and graphMeta
removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodesMap %~ IntMap.delete remNodeId

updateNodeSelection :: NodeIdCollection -> Node -> Node
updateNodeSelection selNodeIds node = node & selected .~ ((node ^. nodeId) `elem` selNodeIds)

-- TODO: nodeRefs and graphMeta
selectNodes :: NodeIdCollection -> State -> State
selectNodes selNodeIds state = state & nodesMap %~ fmap (updateNodeSelection selNodeIds)


addConnection :: PortRef -> PortRef -> State -> (Maybe ConnectionId, State)
addConnection destPortRef = let attachToSelf = (destPortRef ^. refPortId) == PortNum 0 in
    if attachToSelf then addApplication1 destPortRef
                    else addAccessor     destPortRef

removeConnections :: [ConnectionId] -> State -> State
removeConnections connIds state = foldr removeConnection state connIds

removeConnection :: ConnectionId -> State -> State
removeConnection connId state = state & connectionsMap %~ IntMap.delete connId

addAccessor :: PortRef -> PortRef -> State -> (Maybe ConnectionId, State)
addAccessor destPortRef sourcePortRef state =
    let refsMap      = state ^. nodesRefsMap
        destId       = destPortRef   ^. refPortNodeId
        sourceId     = sourcePortRef ^. refPortNodeId
        destRefMay   = IntMap.lookup destId   refsMap
        sourceRefMay = IntMap.lookup sourceId refsMap
    in case (destRefMay, sourceRefMay) of
        (Just destRef, Just sourceRef) -> (Just newConnId, newState)
            where newState = state & graphMeta .~ newGraphMeta
                                   & nodesRefsMap   %~ IntMap.insert (newNode ^. hiddenNodeId) ref
                                   & connectionsMap %~ IntMap.insert (newConnection ^. connId) newConnection
                  (ref, newGraphMeta) = makeAcc newNode destRef sourceRef $ rebuild $ state ^. graphMeta
                  newNode             = HiddenNode Accessor $ genNodeId state
                  newConnection       = Connection newConnId sourcePortRef destPortRef
                  newConnId           = genConnectionId state
        (_, _) -> (Nothing, state)


addApplication1 :: PortRef -> PortRef -> State -> (Maybe ConnectionId, State)
addApplication1 funPortRef argPortRef state =
    let refsMap   = state ^. nodesRefsMap
        funId     = funPortRef ^. refPortNodeId
        argId     = argPortRef ^. refPortNodeId
        funRefMay = IntMap.lookup funId refsMap
        argRefMay = IntMap.lookup argId refsMap
    in case (funRefMay, argRefMay) of
        (Just funRef, Just argRef) -> (Just newConnId, newState)
            where newState = state & graphMeta  .~ newGraphMeta
                                   & nodesRefsMap   %~ IntMap.insert (newNode ^. hiddenNodeId) ref
                                   & connectionsMap %~ IntMap.insert (newConnection ^. connId) newConnection
                  (ref, newGraphMeta) = makeApp1 newNode argRef funRef $ rebuild $ state ^. graphMeta
                  newNode             = HiddenNode Application $ genNodeId state
                  newConnection       = Connection newConnId argPortRef funPortRef
                  newConnId           = genConnectionId state
        (_, _) -> (Nothing, state)


addApplication :: PortRef -> [PortRef] -> State -> State -- TODO: return ([ConnectionId], State)
addApplication funPortRef argPortRefs state =
    let refsMap    = state ^. nodesRefsMap
        argIds :: [NodeId]
        argIds     = (^. refPortNodeId) <$> argPortRefs
        funId      = funPortRef ^. refPortNodeId
        argRefs    = catMaybes $ (flip IntMap.lookup refsMap) <$> argIds
        funRefMay  = IntMap.lookup funId refsMap
    in case funRefMay of
        Just funRef -> state & graphMeta      .~ newGraphMeta
                             & nodesRefsMap   %~ IntMap.insert (newNode ^. hiddenNodeId) ref
                             & connectionsMap %~ IntMap.union newConnections
            where (ref, newGraphMeta) = makeApp newNode funRef argRefs $ rebuild $ state ^. graphMeta
                  newNode             = HiddenNode Application $ genNodeId state
                  availConnectionId   = genConnectionId state
                  availConnectionIds  = [availConnectionId..]
                  newConnections      = IntMap.fromList $
                        (\(connId, argPortRef) -> (connId, Connection connId argPortRef funPortRef)) <$> (zip availConnectionIds argPortRefs)
        _ -> state



-- helpers

makeVar :: Node -> StateGraphMeta -> RefFunctionGraphMeta
makeVar node bldrState = flip runGraphState bldrState $ do
    genTopStar
    withMeta (MetaNode node) $ var $ Text.unpack $ node ^. expression

makeAcc :: HiddenNode -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
makeAcc node destRef sourceRef bldrState = flip runGraphState bldrState $
    withMeta (MetaHiddenNode node) $ sourceRef @. destRef

makeApp1 :: HiddenNode -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
makeApp1 node argRef funRef bldrState = flip runGraphState bldrState $
    withMeta (MetaHiddenNode node) $ funRef @$ [arg argRef]

makeApp :: HiddenNode -> GraphRefMeta -> [GraphRefMeta] -> StateGraphMeta -> RefFunctionGraphMeta
makeApp node funRef argRefs bldrState = flip runGraphState bldrState $ do
    let args = fmap arg argRefs
    withMeta (MetaHiddenNode node) $ funRef @$ args


-- main :: IO ()
-- main = do
--     let (rv1, a) = varA def
--         (rv2, b) = varB $ rebuild a
--         (rf1, c) = accA rv1 $ rebuild b
--         (rv3, d) = appA rf1 rv1 rv2 $ rebuild c
--         (rf2, e) = varF $ rebuild d
--         (rv5, f) = appB rf2 rv3 $ rebuild e
--         (rv6, g) = appA rf1 rv5 rv3 $ rebuild f
--         out      = g
