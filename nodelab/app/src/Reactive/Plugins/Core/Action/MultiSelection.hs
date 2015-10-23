{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.MultiSelection where

import           Utils.PreludePlus
import           Utils.Vector      (Vector2)

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.Widget

import           Event.Keyboard (KeyMods(..))
import qualified Event.Mouse    as Mouse
import qualified Event.Keyboard as Keyboard
import           Event.Event    (Event(Mouse, Keyboard))

import           Reactive.Plugins.Core.Action
import qualified Reactive.State.MultiSelection as MultiSelection
import           Reactive.State.MultiSelection (DragHistory(..))
import qualified Reactive.State.UnderCursor    as UnderCursor
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.Selection      as Selection
import qualified Reactive.State.Camera         as Camera
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State)

import           Reactive.Commands.Command          (Command, performIO)
import           Reactive.Commands.UIRegistry.Focus (focusOnTopNode)
import           Reactive.Commands.Selection        (unselectAll, selectAll, updateSelectionUI)

import           Control.Monad.State                               hiding (State)

toAction :: Event Node -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton _ _)) = Just $ startDrag event
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag

toAction (Keyboard (Keyboard.Event Keyboard.Press 'A'   _)) = Just trySelectAll
toAction (Keyboard (Keyboard.Event Keyboard.Down  '\27' _)) = Just tryUnselectAll
toAction _ = Nothing

trySelectAll :: Command State ()
trySelectAll = do
    focusedWidget <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    when (isNothing focusedWidget) selectAll

tryUnselectAll :: Command State ()
tryUnselectAll = do
    focusedWidget <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    when (isNothing focusedWidget) unselectAll

shouldStartDrag :: Mouse.Event -> Command State Bool
shouldStartDrag (Mouse.Event Mouse.Pressed _ Mouse.LeftButton (KeyMods False False False False) Nothing) = do
    nodesUnderCursor <- gets UnderCursor.getNodesUnderCursor
    portUnderCursor  <- gets UnderCursor.getPortRefUnderCursor
    return $ null nodesUnderCursor && isNothing portUnderCursor
shouldStartDrag _ = return False

startDrag :: Mouse.Event -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ _) = do
    shouldDrag <- shouldStartDrag event
    when shouldDrag $ do
        Global.multiSelection . MultiSelection.history ?= (DragHistory coord coord)
        unselectAll

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.multiSelection . MultiSelection.history
    case dragHistory of
        Nothing                          -> return ()
        Just (DragHistory start current) -> do
            Global.multiSelection . MultiSelection.history ?= DragHistory start coord
            updateSelection start coord
            drawSelectionBox start coord
            focusOnTopNode

updateSelection :: Vector2 Int -> Vector2 Int -> Command State ()
updateSelection start end = do
    nodes  <- use $ Global.graph . Graph.nodes
    camera <- use $ Global.camera . Camera.camera
    Global.selection . Selection.nodeIds .= getNodeIdsIn start end camera nodes
    updateSelectionUI

drawSelectionBox :: Vector2 Int -> Vector2 Int -> Command State ()
drawSelectionBox start end = do
    camera        <- use $ Global.camera . Camera.camera
    let startSelectionBox = Camera.screenToWorkspace camera start
        endSelectionBox   = Camera.screenToWorkspace camera end
    performIO $ UI.displaySelectionBox startSelectionBox endSelectionBox

stopDrag :: Command State ()
stopDrag = do
    Global.multiSelection . MultiSelection.history .= Nothing
    performIO UI.hideSelectionBox
