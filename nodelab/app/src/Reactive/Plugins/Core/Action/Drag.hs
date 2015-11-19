module Reactive.Plugins.Core.Action.Drag where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.UITypes
import           Object.Widget

import           Event.Keyboard hiding        (Event)
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding        (Event, widget)
import qualified Event.Mouse    as Mouse
import           Event.Event

import           Reactive.Commands.Graph
import           Reactive.Commands.Command    (Command, performIO, execCommand)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.Commands.Selection  as Selection
import qualified Reactive.State.Drag          as Drag
import           Reactive.State.Drag          (DragHistory(..))
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.Camera        as Camera
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry
import           Reactive.State.Global        (State)

import qualified BatchConnector.Commands      as BatchCmd
import           Batch.Workspace              (Workspace)

import qualified Object.Widget.Node as Model

import           Control.Monad.State          hiding (State)

import qualified UI.Generic as UI

toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  pos   Mouse.LeftButton (KeyMods False False False False) (Just _))) = Just $ startDrag pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction _                                                                     = Nothing


isNodeUnderCursor :: Command UIRegistry.State Bool
isNodeUnderCursor = do
    maybeOver    <- use UIRegistry.widgetOver
    case maybeOver of
        Just widgetId -> do
            widget <- UIRegistry.lookupTypedM widgetId :: Command UIRegistry.State (Maybe (WidgetFile Model.Node))
            return $ isJust widget
        Nothing       -> return False

startDrag :: Vector2 Int -> Command State ()
startDrag coord = do
    shouldDrag <- zoom Global.uiRegistry isNodeUnderCursor
    when shouldDrag $ do
        Global.drag . Drag.history ?= (DragHistory coord coord coord)

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.drag . Drag.history
    forM_ dragHistory $ \(DragHistory start previous current) -> do
        Global.drag . Drag.history ?= DragHistory start current coord
        moveNodes $ coord - current

moveNodes :: Vector2 Int -> Command State ()
moveNodes delta = do
    widgets <- zoom Global.uiRegistry Selection.selectedNodes
    delta'  <- scaledDelta delta
    let selectedIds = (^. objectId) <$> widgets
    forM_ selectedIds $ \id -> zoom Global.uiRegistry $ UICmd.moveBy id delta'
    updatePortAngles
    updateConnections

stopDrag :: Command State ()
stopDrag = do
    Global.drag . Drag.history .= Nothing

    widgets <- zoom Global.uiRegistry allNodes
    let selected = filter (^. widget . Model.isSelected) widgets
        nodesToUpdate = (\w -> (w ^. widget . Model.nodeId, w ^. widget . widgetPosition)) <$> selected

    nodes <- forM nodesToUpdate $ \(id, pos) -> do
        Global.graph . Graph.nodesMap . ix id . nodePos .= pos
        preuse $ Global.graph . Graph.nodesMap . ix id

    workspace <- use $ Global.workspace
    performIO $ BatchCmd.updateNodes workspace (catMaybes nodes)
    updateConnections
    updatePortAngles

scaledDelta :: Vector2 Int -> Command State (Vector2 Double)
scaledDelta delta = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    return $ (/ factor) . fromIntegral <$> delta
