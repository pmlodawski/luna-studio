module Reactive.Plugins.Core.Action.Connect where

import           Debug.Trace
import           Utils.Angle                  (boundedAngle, toAngle)
import qualified Utils.Nodes                  as NodeUtils
import           Utils.PreludePlus
import           Utils.Vector

import           Object.UITypes
import           Object.Widget                (WidgetFile, parent, widget)
import qualified Object.Widget.Connection     as UIConnection
import qualified Object.Widget.Node           as NodeModel
import qualified Object.Widget.Port           as PortModel

import           Event.Event                  (Event (..))
import           Event.Keyboard               hiding (Event)
import qualified Event.Keyboard               as Keyboard
import           Event.Mouse                  hiding (Event, widget)
import qualified Event.Mouse                  as Mouse

import           Reactive.Commands.Command    (Command, execCommand, performIO)
import           Reactive.Commands.Graph
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Camera        as Camera
import           Reactive.State.Connect       (Connecting (..), DragHistory (..))
import qualified Reactive.State.Connect       as Connect
import           Reactive.State.Global        (State)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified BatchConnector.Commands      as BatchCmd

import qualified Empire.API.Data.Node         as Node
import           Empire.API.Data.Port         (InPort (Self))
import           Empire.API.Data.PortRef      (AnyPortRef (..), InPortRef (..), OutPortRef (..))



toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton _ (Just _))) = Just $ startDrag event
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ whileConnecting $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    _   _                _ _)) = Just $ whileConnecting $ stopDrag'
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just $ whileConnecting $ stopDrag event
toAction _                                                                     = Nothing

showCurrentConnection :: Vector2 Double -> Vector2 Double -> Command UIRegistry.State ()
showCurrentConnection from to = UICmd.update_ UIRegistry.currentConnectionId $ (UIConnection.currentFrom    .~ from)
                                                                             . (UIConnection.currentTo      .~ to  )
                                                                             . (UIConnection.currentVisible .~ True)

setCurrentConnectionColor :: Int -> Command UIRegistry.State ()
setCurrentConnectionColor color = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentColor .~ color

hideCurrentConnection :: Command UIRegistry.State ()
hideCurrentConnection = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentVisible .~ False

getPortWidgetUnderCursor :: EventWidget -> Command UIRegistry.State (Maybe (WidgetFile PortModel.Port))
getPortWidgetUnderCursor (EventWidget widgetId _ _) = do
    file <- UIRegistry.lookupTypedM widgetId
    return file

startDrag :: Mouse.RawEvent -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ (Just evWd)) = do
    sourcePortWd <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
    forM_ sourcePortWd $ \file -> do
        let model = file ^. widget
        let sourceRef = model ^. PortModel.portRef
        graph  <- use Global.graph
        camera <- use $ Global.camera . Camera.camera
        sourceNodePos <- zoom Global.uiRegistry $ UICmd.get (fromJust $ file ^. parent) NodeModel.position
        Global.connect . Connect.connecting ?= (Connecting sourceRef (evWd ^. Mouse.widgetId) (model ^. PortModel.angleVector) sourceNodePos Nothing (DragHistory coord coord))
        zoom Global.uiRegistry $ setCurrentConnectionColor $ model ^. PortModel.color

whileConnecting :: (Connect.Connecting -> Command State ()) -> Command State ()
whileConnecting run = do
    connectingMay <- use $ Global.connect . Connect.connecting
    forM_ connectingMay $ \connecting -> run connecting

handleMove :: Vector2 Int -> Connect.Connecting -> Command State ()
handleMove coord (Connecting sourceRef sourceWidget sourceVector nodePos _ (DragHistory start current)) = do
    graph  <- use Global.graph
    camera <- use $ Global.camera . Camera.camera
    Global.connect . Connect.connecting . _Just . Connect.history . Connect.dragCurrentPos .= coord
    start'   <- zoom Global.camera $ Camera.screenToWorkspaceM start
    current' <- zoom Global.camera $ Camera.screenToWorkspaceM coord
    zoom Global.uiRegistry $ do
        startLine <- case sourceRef of
            (InPortRef' (InPortRef _ Self)) -> return $ nodePos
            _                   -> do
                newVector <- UICmd.get sourceWidget PortModel.angleVector
                portCount <- UICmd.get sourceWidget PortModel.portCount
                let
                    portAngle = toAngle $ newVector
                    angle     = boundedAngle portAngle portCount nodePos current'
                    sx        = (nodePos ^. x) + outerPos * cos angle
                    sy        = (nodePos ^. y) + outerPos * sin angle
                    outerPos  = 22.0
                return $ Vector2 sx sy
        showCurrentConnection startLine current'
    updateConnections

stopDrag' :: Connect.Connecting -> Command State ()
stopDrag' _ = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection (OutPortRef' a) (InPortRef' b) = Just (a, b)
toValidConnection (InPortRef' a) (OutPortRef' b) = Just (b, a)
toValidConnection _ _ = Nothing

stopDrag :: Mouse.RawEvent -> Connect.Connecting -> Command State ()
stopDrag event@(Mouse.Event _ coord _ _ mayEvWd) (Connecting sourceRef _ _ _ _ (DragHistory start current)) = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection

    graph <- use Global.graph
    nodesMap <- use $ Global.graph . Graph.nodesMap
    forM_ mayEvWd $ \evWd -> do
        destinationFile <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
        forM_ destinationFile $ \destinationFile -> do
            let destinationRef = destinationFile ^. widget . PortModel.portRef
            let srcDstMay = toValidConnection sourceRef destinationRef
            forM_ srcDstMay $ \(src, dst) -> do
                batchConnectNodes src dst
    updateConnections
