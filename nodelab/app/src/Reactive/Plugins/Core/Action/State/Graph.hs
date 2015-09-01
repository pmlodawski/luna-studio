module Reactive.Plugins.Core.Action.State.Graph where


import           Utils.PreludePlus
import           Utils.Vector

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Object.Object
import           Object.Port
import           Object.Node

import           Luna.Syntax.Builder.Graph hiding (get, put)
import           Luna.Syntax.Builder
import           AST.AST


type NodesMap = IntMap Meta


data State = State { _nodeList      :: NodeCollection  -- don't access it directly from outside this file!
                   , _nodes         :: NodesMap
                   , _focusedNodeId :: NodeId
                   , _bldrState     :: BldrState GraphMeta
                   } deriving (Show)

makeLenses ''State


-- instance Show State where
--     show a = show $ IntMap.size $ a ^. nodes

instance Eq State where
    a == b = (a ^. nodeList) == (b ^. nodeList) && (a ^. nodes) == (b ^. nodes)

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
addNode newNode state = state & nodeList .~ newNodeList where
    newNodeList = newNode : state ^. nodeList


removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodeList .~ newNodeList where
    newNodeList = filter (\node -> node ^. nodeId /= remNodeId) $ state ^. nodeList



selectNodes :: NodeIdCollection -> State -> State
selectNodes nodeIds state = state & nodeList .~ newNodeList where
    newNodeList = updateNodesSelection nodeIds $ state ^. nodeList
