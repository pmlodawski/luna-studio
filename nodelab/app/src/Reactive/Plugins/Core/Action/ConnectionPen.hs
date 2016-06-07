{-# LANGUAGE TupleSections #-}

module Reactive.Plugins.Core.Action.ConnectionPen where


import           Utils.Angle
import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.IntMap.Lazy                  as IntMap

import qualified JS.ConnectionPen                  as UI
import qualified JS.Widget                         as UI

import           Object.UITypes
import           Object.Widget                     (WidgetFile, widget)
import qualified Object.Widget.Connection          as UIConnection
import qualified Object.Widget.Node                as UINode
import           Empire.API.Data.Node              (NodeId)

import qualified Event.ConnectionPen               as ConnectionPen
import           Event.Event
import           Event.Keyboard                    hiding (Event)
import qualified Event.Keyboard                    as Keyboard
import           Event.Mouse                       hiding (Event, widget)
import qualified Event.Mouse                       as Mouse

import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import qualified Reactive.State.UIRegistry         as UIRegistry

import           Reactive.Commands.Command         (Command, execCommand, performIO)
import           Reactive.Commands.Graph.Disconnect (disconnectAll)
import           Reactive.Commands.Graph
import qualified Reactive.State.ConnectionPen      as ConnectionPen

import qualified Reactive.Commands.Batch           as BatchCmd

import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef(..), OutPortRef(..))

import           Control.Monad.State               hiding (State)
import qualified JS.GoogleAnalytics          as GA


toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton  (KeyMods False True False False) _)) = Just $ startConnecting event
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton  (KeyMods True  True False False) _)) = Just $ startDisconnecting event
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  _   Mouse.RightButton (KeyMods False True False False) _)) = Just $ startDisconnecting event
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton  _ _)) = Just $ whileDrawing $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.RightButton _ _)) = Just $ whileDrawing $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    _   Mouse.NoButton    _ _)) = Just $ whileDrawing stopDrag
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.LeftButton  _ _)) = Just $ whileDrawing stopDrag
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.RightButton _ _)) = Just $ whileDrawing stopDrag
toAction (ConnectionPen (ConnectionPen.Segment widgets))                     = Just $ whileDrawing $ handleAction widgets
toAction _                                                                   = Nothing

startConnecting :: Mouse.RawEvent -> Command State ()
startConnecting event@(Mouse.Event _ coord _ _ _) = do
    performIO $ UI.beginPath coord True
    Global.connectionPen . ConnectionPen.drawing .= Just (ConnectionPen.Drawing coord ConnectionPen.Connecting Nothing [])

startDisconnecting :: Mouse.RawEvent -> Command State ()
startDisconnecting event@(Mouse.Event _ coord _ _ _) = do
    performIO $ UI.beginPath coord False
    Global.connectionPen . ConnectionPen.drawing .= Just (ConnectionPen.Drawing coord ConnectionPen.Disconnecting Nothing [])

whileDrawing :: (ConnectionPen.Drawing -> Command State ()) -> Command State ()
whileDrawing run = do
    drawingMay <- use $ Global.connectionPen . ConnectionPen.drawing
    withJust drawingMay $ \drawing -> run drawing

handleMove :: Vector2 Int -> ConnectionPen.Drawing -> Command State ()
handleMove coord drawing = do
    let previousPos = drawing ^. ConnectionPen.previousPos
    Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.previousPos .= coord
    performIO $ do
        UI.clearCanvas
        UI.drawSegment coord
        UI.requestWidgetsBetween previousPos coord

stopDrag :: ConnectionPen.Drawing -> Command State ()
stopDrag drawing = do
    Global.connectionPen . ConnectionPen.drawing .= Nothing
    performIO UI.endPath

lookupNode :: WidgetId -> Command State (Maybe (WidgetFile UINode.Node))
lookupNode = zoom Global.uiRegistry . UIRegistry.lookupTypedM

lookupConnection :: WidgetId -> Command State (Maybe (WidgetFile UIConnection.Connection))
lookupConnection = zoom Global.uiRegistry . UIRegistry.lookupTypedM

handleAction :: [WidgetId] -> ConnectionPen.Drawing -> Command State ()
handleAction widgets drawing = case drawing ^. ConnectionPen.drawingType of
    ConnectionPen.Connecting    -> handleConnectAction    widgets drawing
    ConnectionPen.Disconnecting -> handleDisconnectAction widgets

handleConnectAction :: [WidgetId] -> ConnectionPen.Drawing -> Command State ()
handleConnectAction widgets drawing = do
    nodesMay <- sequence $ lookupNode <$> widgets
    let nodes = (view $ widget . UINode.nodeId) <$> catMaybes nodesMay
    when (not . null $ nodes) $ do
        let path              = remdups $ (maybeToList $ drawing ^. ConnectionPen.lastNode) ++ nodes
        Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.visitedNodes %= (++ nodes)
        Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.lastNode .= maybeLast path
        let nodesToConnect    = zipAdj path
        autoConnectAll nodesToConnect

handleDisconnectAction :: [WidgetId] -> Command State ()
handleDisconnectAction widgets = do
    connectionsMay <- sequence $ lookupConnection <$> widgets
    let connections = (view $ widget . UIConnection.connectionId) <$> catMaybes connectionsMay
    when (not . null $ connections) $ do
        disconnectAll connections
        GA.sendEvent $ GA.Disconnect

remdups               :: (Eq a) => [a] -> [a]
remdups (x : xx : xs) =  if x == xx then remdups (x : xs) else x : remdups (xx : xs)
remdups xs            = xs

zipAdj x = zip x $ tail x

maybeLast [] = Nothing
maybeLast xs = Just $ last xs

autoConnectAll :: [(NodeId, NodeId)] -> Command State ()
autoConnectAll []    = return ()
autoConnectAll nodes = autoConnectForward (head nodes) -- TODO: forall - foldr

autoConnectForward :: (NodeId, NodeId) -> Command State ()
autoConnectForward (srcNodeId, dstNodeId) = autoConnect (srcNodeId, dstNodeId)

autoConnectBackwards :: (NodeId, NodeId) -> Command State ()
autoConnectBackwards (srcNodeId, dstNodeId) = autoConnect (dstNodeId, srcNodeId)

autoConnect :: (NodeId, NodeId) -> Command State ()
autoConnect (srcNodeId, dstNodeId) = do
    graph     <- use Global.graph
    BatchCmd.connectNodes (OutPortRef srcNodeId Port.All) (InPortRef dstNodeId Port.Self)
    GA.sendEvent $ GA.Connect GA.Pen
