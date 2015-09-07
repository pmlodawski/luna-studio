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


type NodesMap = IntMap GraphRefMeta


type ConnectionsCollections = [(PortRef, PortRef)]

data State = State { _nodeList      :: NodeCollection  -- don't access it directly from outside this file!
                   , _connections   :: ConnectionsCollections -- don't access it directly
                   , _nodeRefs      :: NodesMap
                   , _focusedNodeId :: NodeId
                   , _graphMeta     :: GraphMeta
                   } deriving (Show)

makeLenses ''State


-- instance Show State where
--     show a = show $ IntMap.size $ a ^. nodes

instance Eq State where
    a == b = (a ^. nodeList) == (b ^. nodeList)

instance Default State where
    def = State def def def def def

instance PrettyPrinter NodesMap where
    display nodes =
        "map(" <> show (IntMap.keys nodes)
        <> " " <> show (IntMap.assocs nodes)
        <> ")"

instance PrettyPrinter State where
    display (State nodesList nodes connections focusedNodeId bldrState) =
          "graph(" <> display nodesList
        <> " "     <> display nodes
        <> " "     <> display connections
        <> " "     <> display focusedNodeId
        <> " "     <> show bldrState
        <> ")"


maxNodeId :: NodeCollection -> NodeId
maxNodeId []    = 0
maxNodeId nodes = (^. nodeId) $ maximumBy (on compare (^. nodeId)) nodes

genId :: State -> NodeId
genId state = let refs = state ^. nodeRefs in
    if IntMap.null refs then 0
                        else 1 + (fst $ IntMap.findMax refs)


getNodes :: State -> NodeCollection
getNodes = (^. nodeList)

getConnections :: State -> ConnectionsCollections
getConnections = (^. connections)

updateNodes :: NodeCollection -> State -> State
updateNodes newNodeList state = state & nodeList .~ newNodeList


addNode :: Node -> State -> State
addNode newNode state  = state & nodeList     .~ newNodeList
                               & graphMeta    .~ newGraphMeta
                               & nodeRefs     .~ newNodeRefs
    where (ref, newGraphMeta) = makeVar newNode $ rebuild $ state ^. graphMeta
          newNodeRefs         = IntMap.insert (newNode ^. nodeId) ref $ state ^. nodeRefs
          newNodeList         = newNode : state ^. nodeList



-- TODO: nodeRefs and graphMeta
removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodeList .~ newNodeList
    where newNodeList = filter (\node -> node ^. nodeId /= remNodeId) $ state ^. nodeList


-- TODO: nodeRefs and graphMeta
selectNodes :: NodeIdCollection -> State -> State
selectNodes nodeIds state = state & nodeList .~ newNodeList
    where newNodeList = updateNodesSelection nodeIds $ state ^. nodeList


addAccessor :: NodeId -> NodeId -> State -> State
addAccessor sourceId destId state =
    let refMap = state ^. nodeRefs
        sourceRefMay = IntMap.lookup sourceId refMap
        destRefMay   = IntMap.lookup destId   refMap
    in case (sourceRefMay, destRefMay) of
        (Just sourceRef, Just destRef) -> state & graphMeta    .~ newGraphMeta
                                                & nodeRefs     .~ newNodeRefs
            where (ref, newGraphMeta) = makeAcc newNode sourceRef destRef $ rebuild $ state ^. graphMeta
                  newNodeRefs         = IntMap.insert (newNode ^. hiddenNodeId) ref $ state ^. nodeRefs
                  newNode             = HiddenNode Accessor $ genId state
        (_, _) -> state


addApplication :: PortRef -> PortRef -> State -> State
addApplication funPortRef argPortRef state =
    let refMap    = state ^. nodeRefs
        funId     = funPortRef ^. refPortNode . nodeId
        argId     = argPortRef ^. refPortNode . nodeId
        funRefMay = IntMap.lookup funId refMap
        argRefMay = IntMap.lookup argId refMap
    in case (funRefMay, argRefMay) of
        (Just funRef, Just argRef) -> state & graphMeta    .~ newGraphMeta
                                            & nodeRefs     .~ newNodeRefs
                                            & connections  .~ newConnections
            where (ref, newGraphMeta) = makeApp1 newNode funRef argRef $ rebuild $ state ^. graphMeta
                  newNodeRefs         = IntMap.insert (newNode ^. hiddenNodeId) ref $ state ^. nodeRefs
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
