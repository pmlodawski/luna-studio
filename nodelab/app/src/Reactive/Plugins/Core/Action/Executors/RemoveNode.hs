module Reactive.Plugins.Core.Action.Executors.RemoveNode where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.Selection  as Selection
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.Bindings             as UI
import qualified JS.Widget               as UI
import qualified JS.NodeGraph            as UIGraph
import           Object.Node
import           Object.Widget.Helpers   (nodeIdToWidgetId)

removeNode :: Node -> Char -> State -> (State, IO ())
removeNode node key state = case key of
    'r' -> performRemoval node state
    _   -> (state, return ())

performRemoval :: Node -> State -> (State, IO ())
performRemoval node state = (newState, action) where
    action       =  maybe (return ()) UI.removeWidget nodeWidgetId
                 >> BatchCmd.removeNodeById (state ^. Global.workspace) (node ^. nodeId)
                 >> mapM_ UIGraph.setNodeFocused topNodeId
    newState     = state & Global.graph      .~ newGraph
                         & Global.selection  .~ newSelection
                         & Global.uiRegistry . UIRegistry.focusedWidget .~ topWidgetId
    newGraph     = Graph.removeNode (node ^. nodeId) (state ^. Global.graph)
    newSelection = (state ^. Global.selection) & Selection.nodeIds %~ drop 1
    topNodeId    = newSelection ^? Selection.nodeIds . ix 0
    topWidgetId  = topNodeId >>= nodeIdToWidgetId uiRegistry
    uiRegistry   = state ^. Global.uiRegistry
    nodeWidgetId = nodeIdToWidgetId uiRegistry $ node ^. nodeId
