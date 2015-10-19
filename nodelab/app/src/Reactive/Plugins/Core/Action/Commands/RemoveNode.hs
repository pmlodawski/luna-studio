module Reactive.Plugins.Core.Action.Commands.RemoveNode where

import           Utils.PreludePlus
import           Event.Keyboard (KeyMods)
import           Object.UITypes (WidgetId)
import           Reactive.Plugins.Core.Action.State.Global             (State)
import qualified Reactive.Plugins.Core.Action.State.Global             as Global
import qualified Reactive.Plugins.Core.Action.State.Selection          as Selection
import qualified Reactive.Plugins.Core.Action.State.UIRegistry         as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Graph              as Graph
import           Reactive.Plugins.Core.Action.Commands.Command         (Command, performIO)
import           Reactive.Plugins.Core.Action.Commands.DisconnectNodes (localDisconnectAll)

import           Reactive.Plugins.Core.Action.Commands.UIRegistry.RemoveWidget (removeWidgets)
import           Reactive.Plugins.Core.Action.Commands.UIRegistry.Focus        (focusOnTopNode)

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph            as UIGraph
import           Object.Node             (Node, nodeId)
import           Object.Widget.Helpers   (nodeIdToWidgetId)

removeNode :: Node -> Char -> KeyMods -> WidgetId -> Command State ()
removeNode node key _ _ = case key of
    '\x08' -> performRemoval node
    '\x2e' -> performRemoval node
    _         -> return ()

performRemoval :: Node -> Command State ()
performRemoval node = do
    danglingConns <- uses Global.graph (Graph.connectionIdsContainingNode $ node ^. nodeId)
    localDisconnectAll danglingConns
    Global.graph %= Graph.removeNode (node ^. nodeId)
    Global.selection . Selection.nodeIds %= drop 1

    uiRegistry <- use Global.uiRegistry
    workspace  <- use Global.workspace

    let nodeWidgetId = maybeToList $ nodeIdToWidgetId uiRegistry $ node ^. nodeId
    zoom Global.uiRegistry $ removeWidgets nodeWidgetId
    focusOnTopNode
    performIO $ do
        BatchCmd.removeNodeById workspace (node ^. nodeId)
