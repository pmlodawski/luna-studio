module Reactive.Commands.UIRegistry.Focus where

import           Utils.PreludePlus
import           Control.Monad.State   hiding (State)
import           Object.Object         (NodeId)
import qualified Object.Widget.Node as Model
import           Object.Widget.Helpers (nodeIdToWidgetId)
import qualified JS.NodeGraph          as UIGraph

import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.State.UIRegistry (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Selection  as Selection
import           Reactive.Commands.Command (Command, performIO)
import qualified UI.Widget.Node as UINode

focusOnNode :: NodeId -> Command (State a) ()
focusOnNode nodeId = do
    uiRegistry <- get
    let widgetId = nodeIdToWidgetId uiRegistry nodeId
    performIO $ putStrLn "Focus.hs: focusOnNode"
    UIRegistry.focusedWidget .= widgetId
    -- model <- UIRegistry.updateWidgetM widgetId $ Model.isFocused .~ True
    --
    -- UINode.updateSelectedState widgetId model


focusOnTopNode :: Command Global.State ()
focusOnTopNode = do
    performIO $ putStrLn "Focus.hs: focusOnTopNode"
    -- topNode <- preuse $ Global.selection . Selection.nodeIds . ix 0
    -- case topNode of
    --     Just nodeId -> zoom Global.uiRegistry $ focusOnNode nodeId
    --     _           -> return ()

