module Reactive.Plugins.Core.Action.State.Graph where


import           Utils.PreludePlus
import           Utils.Vector

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Object.Object
import           Object.Port
import           Object.Node

import           AST.AST


type NodesMap = IntMap Meta


data State = State { _nodeList  :: NodeCollection
                   , _nodes     :: NodesMap
                   } deriving (Eq, Show)


-- instance Show State where
--     show a = show $ IntMap.size $ a ^. nodes

makeLenses ''State

instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State nodesList nodes) =
           "nM("        <> show nodesList
        <> ", "         <> show (IntMap.keys nodes)
        <> ")"


getNodes :: State -> NodeCollection
getNodes = (^. nodeList)

updateNodes :: NodeCollection -> State -> State
updateNodes newNodeList state = state & nodeList .~ newNodeList
