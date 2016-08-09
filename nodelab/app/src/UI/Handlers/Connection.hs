module UI.Handlers.Connection where

import           Utils.PreludePlus

import           Data.HMap.Lazy                     (TypeKey (..))
import           Utils.Vector

import           Empire.API.Data.Connection         (Connection (..))
import qualified Empire.API.Data.Connection         as Connection
import qualified Empire.API.Data.PortRef            as PortRef

import           Event.Event                        (JSState)
import qualified Event.Mouse                        as Mouse
import           Object.Widget                      (ResizableWidget, DragMoveHandler, UIHandlers, WidgetId, mousePressed, mouseMove, mouseOut, dragMove, startPos)
import qualified Object.Widget.Connection           as Model
import qualified Object.Widget.Node                 as NodeModel
import qualified Object.Widget.Port                 as PortModel
import           Reactive.Commands.Command          (Command, performIO)
import           Reactive.Commands.Graph.Disconnect (disconnectAll)
import qualified Reactive.Commands.UIRegistry       as UICmd
import qualified Reactive.State.Connect             as Connect
import           Reactive.State.Global              (inRegistry)
import qualified Reactive.State.Global              as Global
import qualified Reactive.State.Graph               as Graph
import qualified Reactive.State.UIRegistry          as UIRegistry
import           Reactive.Plugins.Core.Action.Connect (handleMove)

import           UI.Generic                         (defaultResize, startDrag, abortDrag)

data ConnectionEnd = Source | Destination
newtype DragConnectionEndHandler = DragConnectionEndHandler (WidgetId -> ConnectionEnd -> Command UIRegistry.State ())

triggerDragConnectionEndHandler :: WidgetId -> ConnectionEnd -> Command UIRegistry.State ()
triggerDragConnectionEndHandler id end = do
    let key = TypeKey :: TypeKey DragConnectionEndHandler
    maybeHandler <- UICmd.handler id key
    withJust maybeHandler $ \(DragConnectionEndHandler handler) -> handler id end

setCurrentConnectionColor :: Int -> Command UIRegistry.State ()
setCurrentConnectionColor color = UICmd.update_ UIRegistry.currentConnectionId $ Model.currentColor .~ color

endCoeff :: Double
endCoeff = 0.2


mousePressedHandler :: Mouse.Event' -> JSState -> WidgetId -> Command Global.State ()
mousePressedHandler evt _ id = startDrag evt id


dragHandler :: DragMoveHandler Global.State
dragHandler ds _ id = do
    let mouseX = ds ^. startPos . x

    when (mouseX > endCoeff) $ do
        connId <- inRegistry $ UICmd.get id Model.connectionId
        connectionColor <- inRegistry $ UICmd.get id Model.color
        (Just srcPortRef)   <- preuse $ Global.graph . Graph.connectionsMap . ix connId . Connection.src
        (Just portWidgetId) <- use $ Global.graph . Graph.portWidgetsMap . at (PortRef.OutPortRef' srcPortRef)
        (Just nodeWidgetId) <- use $ Global.graph . Graph.nodeWidgetsMap . at (srcPortRef ^. PortRef.srcNodeId)
        disconnectAll [connId]
        sourceNodePos   <- inRegistry $ UICmd.get nodeWidgetId NodeModel.position
        sourcePortAngle <- inRegistry $ UICmd.get portWidgetId PortModel.angleVector
        let coord = floor <$> sourceNodePos + Vector2 10 10
        Global.connect . Connect.connecting ?= (Connect.Connecting (PortRef.OutPortRef' srcPortRef) sourcePortAngle sourceNodePos Nothing)
        zoom Global.uiRegistry $ setCurrentConnectionColor $ connectionColor
        return ()
    when (mouseX < (-endCoeff)) $ do
        connId <- inRegistry $ UICmd.get id Model.connectionId
        connectionColor <- inRegistry $ UICmd.get id Model.color
        (Just dstPortRef) <- preuse $ Global.graph . Graph.connectionsMap . ix connId . Connection.dst
        (Just portWidgetId) <- use $ Global.graph . Graph.portWidgetsMap . at (PortRef.InPortRef' dstPortRef)
        (Just nodeWidgetId) <- use $ Global.graph . Graph.nodeWidgetsMap . at (dstPortRef ^. PortRef.dstNodeId)
        disconnectAll [connId]
        dstNodePos   <- inRegistry $ UICmd.get nodeWidgetId NodeModel.position
        dstPortAngle <- inRegistry $ UICmd.get portWidgetId PortModel.angleVector
        let coord = floor <$> dstNodePos + Vector2 10 10
        Global.connect . Connect.connecting ?= (Connect.Connecting (PortRef.InPortRef' dstPortRef) dstPortAngle dstNodePos Nothing)
        zoom Global.uiRegistry $ setCurrentConnectionColor $ connectionColor
        return ()
    abortDrag

onMouseMove :: Mouse.Event' -> JSState -> WidgetId -> Command Global.State()
onMouseMove evt _ id  = inRegistry $ do
    let mouseX = evt ^. Mouse.position . x
    when (mouseX > endCoeff) $ do
        UICmd.update_ id $ Model.highlight .~ Model.SrcHighlight
    when (mouseX < (-endCoeff)) $ do
        UICmd.update_ id $ Model.highlight .~ Model.DstHighlight

onMouseOut :: WidgetId -> Command Global.State ()
onMouseOut  id = inRegistry $ do
    UICmd.update_ id $ Model.highlight .~ Model.None

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & mouseMove .~ onMouseMove
                     & mouseOut  .~ const onMouseOut
                     & mousePressed  .~ mousePressedHandler
                     & dragMove  .~ dragHandler

