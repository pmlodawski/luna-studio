module Reactive.Commands.AutoLayout where

import           Utils.PreludePlus
import           Object.Object          (NodeId)
import           Object.Node            (Node, NodesMap, nodePos, nodeId)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Utils.Vector           (Vector2)
import           Utils.Graph.AutoLayout (autoLayout)
import           Control.Monad.State    hiding (State)

import           Reactive.Commands.Command (Command, performIO)
import           Reactive.Commands.Graph   (moveNodesUI)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import qualified BatchConnector.Commands                       as BatchCmd

layoutGraph :: Command State ()
layoutGraph = do
    newNodes  <- zoom Global.graph moveNodes
    workspace <- use Global.workspace
    performIO $ BatchCmd.updateNodes workspace newNodes

moveNodes :: Command Graph.State [Node]
moveNodes = do
    nodes       <- gets Graph.getNodes
    connections <- gets Graph.getConnections
    nodesMap    <- gets Graph.getNodesMap
    let newPositions = autoLayout (view nodeId <$> nodes)
                                  (Graph.connectionToNodeIds <$> connections)
                                  150.0
                                  150.0
    let newNodesMap = updatePosition newPositions <$> nodesMap
    modify $ Graph.updateNodes newNodesMap
    performIO $ moveNodesUI newNodesMap
    gets Graph.getNodes

updatePosition :: Map NodeId (Vector2 Double) -> Node -> Node
updatePosition newPositions node = node & nodePos .~ Map.findWithDefault (node ^. nodePos)
                                                                         (node ^. nodeId)
                                                                         newPositions

