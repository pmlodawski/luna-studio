module Reactive.Plugins.Core.Action.Commands.RemoveNode where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.Selection  as Selection
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import           Reactive.Plugins.Core.Action.Commands.Command (Command, performIO)

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.Bindings             as UI
import qualified JS.Widget               as UI
import qualified JS.NodeGraph            as UIGraph
import           Object.Node             (Node, nodeId)
import           Object.Widget.Helpers   (nodeIdToWidgetId)

removeNode :: Node -> Char -> Command State ()
removeNode node key = case key of
    'r' -> performRemoval node
    _   -> return ()

performRemoval :: Node -> Command State ()
performRemoval node = do
    Global.graph %= Graph.removeNode (node ^. nodeId)
    Global.selection . Selection.nodeIds %= drop 1

    uiRegistry <- use Global.uiRegistry
    topNodeId  <- preuse $ Global.selection . Selection.nodeIds . ix 0
    workspace  <- use Global.workspace

    let topWidgetId = topNodeId >>= nodeIdToWidgetId uiRegistry
    let nodeWidgetId = nodeIdToWidgetId uiRegistry $ node ^. nodeId

    Global.uiRegistry . UIRegistry.focusedWidget .= topWidgetId

    performIO $ do
        maybe (return ()) UI.removeWidget nodeWidgetId
        BatchCmd.removeNodeById workspace (node ^. nodeId)
        mapM_ UIGraph.setNodeFocused topNodeId
