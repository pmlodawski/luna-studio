module Reactive.Commands.UIRegistry.Focus where

import           Utils.PreludePlus
import           Control.Monad.State   hiding (State)
import           Object.Object         (NodeId)
import           Object.Widget.Helpers (nodeIdToWidgetId)
import qualified JS.NodeGraph          as UIGraph

import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.State.UIRegistry (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Selection  as Selection
import           Reactive.Commands.Command (Command, performIO)

focusOnNode :: NodeId -> Command (State a) ()
focusOnNode nodeId = do
    uiRegistry <- get
    let widgetId = nodeIdToWidgetId uiRegistry nodeId
    UIRegistry.focusedWidget .= widgetId
    performIO $ UIGraph.setNodeFocused nodeId

focusOnTopNode :: Command Global.State ()
focusOnTopNode = do
    topNode <- preuse $ Global.selection . Selection.nodeIds . ix 0
    case topNode of
        Just nodeId -> zoom Global.uiRegistry $ focusOnNode nodeId
        _           -> return ()

