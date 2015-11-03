{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.MultiSelection where

import           Utils.PreludePlus
import           Utils.Vector      (Vector2)

import           JS.MultiSelection (displaySelectionBox, hideSelectionBox)
import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.Widget
import qualified Object.Widget.Node as NodeModel

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
import qualified Reactive.Commands.UIRegistry       as UICmd
import           Reactive.Commands.Graph (getNodesInRect)
import           Reactive.Commands.Selection        (unselectAll, selectAll)

import           Control.Monad.State                               hiding (State)

toAction :: Event -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  pos Mouse.LeftButton (KeyMods False False False False) Nothing)) = Just $ startDrag pos
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag

toAction (Keyboard (Keyboard.Event Keyboard.Press 'A'   _)) = Just trySelectAll
toAction (Keyboard (Keyboard.Event Keyboard.Down  '\27' _)) = Just tryUnselectAll
toAction _ = Nothing

trySelectAll :: Command State ()
trySelectAll = zoom Global.uiRegistry $ do
    focusedWidget <- use UIRegistry.focusedWidget
    when (isNothing focusedWidget) selectAll

tryUnselectAll :: Command State ()
tryUnselectAll = zoom Global.uiRegistry $ do
    focusedWidget <- use UIRegistry.focusedWidget
    when (isNothing focusedWidget) unselectAll

startDrag :: Vector2 Int -> Command State ()
startDrag coord = do
    Global.multiSelection . MultiSelection.history ?= (DragHistory coord coord)
    zoom Global.uiRegistry $ unselectAll

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
    ids <- getNodesInRect start end
    zoom Global.uiRegistry unselectAll
    forM_ ids $ \id -> do
        zoom Global.uiRegistry $ UICmd.update id (NodeModel.isSelected .~ True)

drawSelectionBox :: Vector2 Int -> Vector2 Int -> Command State ()
drawSelectionBox start end = do
    camera        <- use $ Global.camera . Camera.camera
    let startSelectionBox = Camera.screenToWorkspace camera start
        endSelectionBox   = Camera.screenToWorkspace camera end
    performIO $ displaySelectionBox startSelectionBox endSelectionBox

stopDrag :: Command State ()
stopDrag = do
    Global.multiSelection . MultiSelection.history .= Nothing
    performIO hideSelectionBox
