module Reactive.Plugins.Core.Action.Commands.AutoLayout where

import           Utils.PreludePlus
import           Object.Node            (Node, NodesMap, nodePos, nodeId)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Utils.Vector           (Vector2)
import           Utils.Graph.AutoLayout (autoLayout)

import           Reactive.Plugins.Core.Action.Commands.Command (Command, performIO)
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified BatchConnector.Commnads                       as BatchCmd

layoutGraph :: Command State ()
layoutGraph = do
    newNodes  <- moveNodes
    workspace <- use Global.workspace
    performIO $ BatchCmd.updateNodes workspace newNodes

moveNodes :: Command Graph.State NodesMap
moveNodes = do
    nodes       <- gets Graph.getNodes
    connections <- gets Graph.getConnections
    nodesMap    <- gets Graph.getNodesMap
    let newPositions = autoLayout nodes connections 150.0 150.0
    modify $ Graph.updateNodes (updatePosition newPositions <$> nodesMap)
    get Graph.getNodesMap

updatePosition :: Map NodeId (Vector2 Double) -> Node -> Node
updatePosition newPositions node = node & nodePos .~ Map.findWithDefault (node ^. nodePos)
                                                                         (node ^. nodeId)
                                                                         newPositions

