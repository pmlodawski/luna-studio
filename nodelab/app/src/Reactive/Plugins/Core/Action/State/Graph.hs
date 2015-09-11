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

type ConnectionsCollections = [(PortRef, PortRef)]

data State = State { _nodesMap      :: NodesMap               -- don't access it directly
                   , _connections   :: ConnectionsCollections -- don't access it directly
                   , _nodesRefsMap  :: NodesRefsMap
                   , _focusedNodeId :: NodeId
                   , _graphMeta     :: GraphMeta
                   } deriving (Show)

makeLenses ''State



-- TODO: Implement in full
instance Eq State where
    a == b = (a ^. nodesMap) == (b ^. nodesMap)

instance Default State where
    def = State def def def def def

instance PrettyPrinter NodesRefsMap where
    display nodes =
        "map(" <> show (IntMap.keys nodes)
        <> " " <> show (IntMap.assocs nodes)
        <> ")"

instance PrettyPrinter State where
    display (State nodesMap connections nodesRefsMap focusedNodeId bldrState) =
          "graph(" <> show nodesMap
        <> " "     <> display connections
        <> " "     <> display nodesRefsMap
        <> " "     <> display focusedNodeId
        <> " "     <> show bldrState
        <> ")"


genId :: State -> NodeId
genId state = let refs = state ^. nodesRefsMap in
    if IntMap.null refs then 0
                        else 1 + (fst $ IntMap.findMax refs)

getNode :: State -> NodeId -> Node
getNode state nodeId = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId $ state ^. nodesMap

getNodes :: State -> NodeCollection
getNodes state = IntMap.elems $ state ^. nodesMap

getNodesMap :: State -> NodesMap
getNodesMap = (^. nodesMap)

getConnections :: State -> ConnectionsCollections
getConnections = (^. connections)

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


addConnection :: PortRef -> PortRef -> State -> State
addConnection sourcePortRef destPortRef = let attachToSelf = (destPortRef ^. refPortId) == PortNum 0 in
    if attachToSelf then addApplication sourcePortRef destPortRef
                    else addAccessor    sourcePortRef destPortRef


addAccessor :: PortRef -> PortRef -> State -> State
addAccessor sourcePortRef destPortRef state =
    let refsMap      = state ^. nodesRefsMap
        sourceId     = sourcePortRef ^. refPortNodeId
        destId       = destPortRef   ^. refPortNodeId
        sourceRefMay = IntMap.lookup sourceId refsMap
        destRefMay   = IntMap.lookup destId   refsMap
    in case (sourceRefMay, destRefMay) of
        (Just sourceRef, Just destRef) -> state & graphMeta    .~ newGraphMeta
                                                & nodesRefsMap %~ IntMap.insert (newNode ^. hiddenNodeId) ref
            where (ref, newGraphMeta) = makeAcc newNode sourceRef destRef $ rebuild $ state ^. graphMeta
                  newNode             = HiddenNode Accessor $ genId state
        (_, _) -> state


addApplication :: PortRef -> PortRef -> State -> State
addApplication argPortRef funPortRef state =
    let refsMap   = state ^. nodesRefsMap
        argId     = argPortRef ^. refPortNodeId
        funId     = funPortRef ^. refPortNodeId
        argRefMay = IntMap.lookup argId refsMap
        funRefMay = IntMap.lookup funId refsMap
    in case (funRefMay, argRefMay) of
        (Just funRef, Just argRef) -> state & graphMeta    .~ newGraphMeta
                                            & nodesRefsMap %~ IntMap.insert (newNode ^. hiddenNodeId) ref
                                            & connections  .~ newConnections
            where (ref, newGraphMeta) = makeApp1 newNode funRef argRef $ rebuild $ state ^. graphMeta
                  newNode             = HiddenNode Application $ genId state
                  newConnections      = (funPortRef, argPortRef) : (state ^. connections) -- TODO: move to AST
        (_, _) -> state



-- helpers

makeVar :: Node -> StateGraphMeta -> RefFunctionGraphMeta
makeVar node bldrState = flip runGraphState bldrState $ do
    genTopStar
    withMeta (MetaNode node) $ var $ Text.unpack $ node ^. expression

makeAcc :: HiddenNode -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
makeAcc node sourceRef destRef bldrState = flip runGraphState bldrState $
    withMeta (MetaHiddenNode node) $ sourceRef @. destRef

makeApp1 :: HiddenNode -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
makeApp1 node funRef argRef bldrState = flip runGraphState bldrState $
    withMeta (MetaHiddenNode node) $ funRef @$ [arg argRef]


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
