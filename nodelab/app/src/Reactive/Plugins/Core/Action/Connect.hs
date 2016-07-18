module Reactive.Plugins.Core.Action.Connect
    ( toAction
    , handleMove
    ) where

import           Utils.Angle                     (boundedAngle, toAngle)
import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget                   (WidgetFile, parent, widget)
import qualified Object.Widget.Connection        as UIConnection
import qualified Object.Widget.Node              as NodeModel
import qualified Object.Widget.Port              as PortModel

import           Event.Event                     (Event (..))
import           Event.Keyboard                  hiding (Event)
import           Event.Mouse                     hiding (Event, widget)
import qualified Event.Mouse                     as Mouse

import           Reactive.Commands.Command       (Command)
import           Reactive.Commands.Graph         (portRefToWidgetId)
import           Reactive.Commands.Graph.Connect (batchConnectNodes)
import qualified Reactive.Commands.UIRegistry    as UICmd
import qualified Reactive.State.Camera           as Camera
import           Reactive.State.Connect          (Connecting (..))
import qualified Reactive.State.Connect          as Connect
import           Reactive.State.Global           (State, inRegistry)
import qualified Reactive.State.Global           as Global
import qualified Reactive.State.Graph            as Graph
import qualified Reactive.State.UIRegistry       as UIRegistry

import           Empire.API.Data.Port            (InPort (Self))
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef         as PortRef (dstNodeId, srcNodeId)
import qualified JS.GoogleAnalytics              as GA



toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton (KeyMods False False False False) (Just _))) = Just $ startDrag event
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ whileConnecting $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    _   _                _ _)) = Just $ whileConnecting $ stopDrag'
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just $ whileConnecting $ stopDrag event
toAction _                                                                     = Nothing

showCurrentConnection :: Vector2 Double -> Vector2 Double -> Bool -> Command UIRegistry.State ()
showCurrentConnection from to arrow = UICmd.update_ UIRegistry.currentConnectionId $ (UIConnection.currentFrom    .~ from)
                                                                                   . (UIConnection.currentTo      .~ to  )
                                                                                   . (UIConnection.currentVisible .~ True)
                                                                                   . (UIConnection.currentArrow   .~ arrow)

setCurrentConnectionColor :: Int -> Command UIRegistry.State ()
setCurrentConnectionColor color = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentColor .~ color

hideCurrentConnection :: Command UIRegistry.State ()
hideCurrentConnection = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentVisible .~ False

getPortWidgetUnderCursor :: EventWidget -> Command UIRegistry.State (Maybe (WidgetFile PortModel.Port))
getPortWidgetUnderCursor (EventWidget widgetId _ _) = UIRegistry.lookupTypedM widgetId

startDrag :: Mouse.RawEvent -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ (Just evWd)) = do
    sourcePortWd <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
    withJust sourcePortWd $ \file -> do
        let model = file ^. widget
        let sourceRef = model ^. PortModel.portRef
        graph  <- use Global.graph
        camera <- use $ Global.camera . Camera.camera
        nodeWidget <- inRegistry $ UICmd.parent (fromJust $ file ^. parent)
        sourceNodePos <- inRegistry $ UICmd.get nodeWidget NodeModel.position
        Global.connect . Connect.connecting ?= (Connecting sourceRef (model ^. PortModel.angleVector) sourceNodePos Nothing)
        zoom Global.uiRegistry $ setCurrentConnectionColor $ model ^. PortModel.color

whileConnecting :: (Connect.Connecting -> Command State ()) -> Command State ()
whileConnecting run = do
    connectingMay <- use $ Global.connect . Connect.connecting
    withJust connectingMay $ \connecting -> run connecting

handleMove :: Vector2 Int -> Connect.Connecting -> Command State ()
handleMove coord (Connecting sourceRef sourceVector nodePos _) = do
    graph  <- use Global.graph
    camera <- use $ Global.camera . Camera.camera
    current' <- zoom Global.camera $ Camera.screenToWorkspaceM coord
    startLine <- case sourceRef of
            (InPortRef' (InPortRef _ Self)) -> return nodePos
            _                   -> do
                sourceWidget <- portRefToWidgetId sourceRef
                case sourceWidget of
                    Just sourceWidget -> inRegistry $ do
                        newVector <- UICmd.get sourceWidget PortModel.angleVector
                        portCount <- UICmd.get sourceWidget PortModel.portCount
                        let
                            portAngle = toAngle $ newVector
                            angle     = boundedAngle portAngle portCount nodePos current'
                            sx        = (nodePos ^. x) + outerPos * cos angle
                            sy        = (nodePos ^. y) + outerPos * sin angle
                            outerPos  = 22.0
                        return $ Vector2 sx sy
                    Nothing -> return nodePos
    inRegistry $ case sourceRef of
        InPortRef'   (InPortRef _ Self) -> showCurrentConnection current' startLine False
        InPortRef'   (InPortRef _ _)    -> showCurrentConnection current' startLine True
        OutPortRef'  _                  -> showCurrentConnection startLine current' True

stopDrag' :: Connect.Connecting -> Command State ()
stopDrag' _ = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection a b = (normalize a b) >>= toOtherNode where
    normalize (OutPortRef' a) (InPortRef' b) = Just (a, b)
    normalize (InPortRef' a) (OutPortRef' b) = Just (b, a)
    normalize _ _ = Nothing
    toOtherNode (a, b)
        | a ^. PortRef.srcNodeId /= b ^. PortRef.dstNodeId = Just (a, b)
        | otherwise                                        = Nothing

stopDrag :: Mouse.RawEvent -> Connect.Connecting -> Command State ()
stopDrag event@(Mouse.Event _ coord _ _ mayEvWd) (Connecting sourceRef _ _ _) = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection

    graph <- use Global.graph
    nodesMap <- use $ Global.graph . Graph.nodesMap
    withJust mayEvWd $ \evWd -> do
        destinationFile <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
        withJust destinationFile $ \destinationFile -> do
            let destinationRef = destinationFile ^. widget . PortModel.portRef
            let srcDstMay = toValidConnection sourceRef destinationRef
            withJust srcDstMay $ \(src, dst) -> do
                batchConnectNodes src dst
                GA.sendEvent $ GA.Connect GA.Manual
