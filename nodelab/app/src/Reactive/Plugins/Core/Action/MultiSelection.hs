{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.MultiSelection where

import           Utils.PreludePlus
import           Utils.Vector      (Vector2)

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.Widget
import           Object.Widget.Helpers (nodeIdToWidgetId)

import           Event.Keyboard (KeyMods(..))
import qualified Event.Mouse    as Mouse
import           Event.Event    (Event(Mouse))

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.MultiSelection as MultiSelection
import           Reactive.Plugins.Core.Action.State.MultiSelection (DragHistory(..))
import qualified Reactive.Plugins.Core.Action.State.UnderCursor    as UnderCursor
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection      as Selection
import qualified Reactive.Plugins.Core.Action.State.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.State.Global         (State)

import           Reactive.Plugins.Core.Action.Commands.Command     (Command, performIO)

import           Control.Monad.State                               hiding (State)

toAction :: Event Node -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton _ _)) = Just $ startDrag event
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction _ = Nothing

shouldStartDrag :: Mouse.Event -> Command State Bool
shouldStartDrag (Mouse.Event Mouse.Pressed _ Mouse.LeftButton (KeyMods False False False False) Nothing) = do
    nodesUnderCursor <- gets UnderCursor.getNodesUnderCursor
    portUnderCursor  <- gets UnderCursor.getPortRefUnderCursor
    return $ null nodesUnderCursor && isNothing portUnderCursor
shouldStartDrag _ = return False

startDrag :: Mouse.Event -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ _) = do
    shouldDrag <- shouldStartDrag event
    if shouldDrag
        then Global.multiSelection . MultiSelection.history ?= (DragHistory coord coord)
        else return ()

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.multiSelection . MultiSelection.history
    case dragHistory of
        Nothing                          -> return ()
        Just (DragHistory start current) -> do
            Global.multiSelection . MultiSelection.history ?= DragHistory start coord
            updateSelection start coord
            updateFocusedWidget
            updateSelectionUI start coord

updateSelection :: Vector2 Int -> Vector2 Int -> Command State ()
updateSelection start end = do
    nodes  <- use $ Global.graph . Graph.nodes
    camera <- use $ Global.camera . Camera.camera
    Global.selection . Selection.nodeIds .= getNodeIdsIn start end camera nodes

updateFocusedWidget :: Command State ()
updateFocusedWidget = do
    topNode <- preuse $ Global.selection . Selection.nodeIds . ix 0
    uiRegistry <- use $ Global.uiRegistry
    let topNodeWidget = topNode >>= (nodeIdToWidgetId uiRegistry)
    Global.uiRegistry . UIRegistry.focusedWidget .= topNodeWidget

updateSelectionUI :: Vector2 Int -> Vector2 Int -> Command State ()
updateSelectionUI start end = do
    camera        <- use $ Global.camera . Camera.camera
    selectedNodes <- use $ Global.selection . Selection.nodeIds
    allNodes      <- use $ Global.graph . Graph.nodes
    let startSelectionBox = Camera.screenToWorkspace camera start
        endSelectionBox   = Camera.screenToWorkspace camera end
        unselectedNodes   = ((view nodeId) <$> allNodes) \\ selectedNodes
        topNode           = selectedNodes ^? ix 0
    performIO $ do
        UI.displaySelectionBox startSelectionBox endSelectionBox
        UI.unselectNodes unselectedNodes
        UI.selectNodes   selectedNodes
        mapM_ UI.setNodeFocused topNode

stopDrag :: Command State ()
stopDrag = do
    Global.multiSelection . MultiSelection.history .= Nothing
    performIO UI.hideSelectionBox
