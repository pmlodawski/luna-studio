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


data State = State { _nodeList      :: NodeCollection  -- don't access it directly from outside this file!
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
    def = State def def def def

instance PrettyPrinter State where
    display (State nodesList nodes focusedNodeId bldrState) =
          "graph(" <> show nodesList
        <> " "     <> show (IntMap.keys nodes)
        <> " "     <> show focusedNodeId
        <> " "     <> show bldrState
        <> ")"


getNodes :: State -> NodeCollection
getNodes = (^. nodeList)

updateNodes :: NodeCollection -> State -> State
updateNodes newNodeList state = state & nodeList .~ newNodeList


addNode :: Node -> State -> State
addNode newNode state  = trace ("graphMeta " <> show newGraphMeta <> " ref " <> show ref) $ state & nodeList     .~ newNodeList
                               & graphMeta    .~ newGraphMeta
                               & nodeRefs     .~ newNodeRefs
    where newNodeList         = newNode : state ^. nodeList
          (ref, newGraphMeta) = makeVar newNode $ rebuild $ state ^. graphMeta
          newNodeRefs         = IntMap.insert (newNode ^. nodeId) ref $ state ^. nodeRefs


makeVar :: Node -> StateGraphMeta -> RefFunctionGraphMeta
makeVar node bldrState = flip runGraphState bldrState $ do
    genTopStar
    withMeta (Meta node) $ var $ show $ node ^. expression



removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodeList .~ newNodeList
    where newNodeList = filter (\node -> node ^. nodeId /= remNodeId) $ state ^. nodeList



selectNodes :: NodeIdCollection -> State -> State
selectNodes nodeIds state = state & nodeList .~ newNodeList
    where newNodeList = updateNodesSelection nodeIds $ state ^. nodeList


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
