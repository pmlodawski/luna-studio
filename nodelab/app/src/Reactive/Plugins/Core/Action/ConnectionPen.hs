{-# LANGUAGE TupleSections #-}

module Reactive.Plugins.Core.Action.ConnectionPen where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import qualified Utils.MockHelper         as MockHelper

import qualified Data.IntMap.Lazy         as IntMap

import qualified JS.Widget                as UI
import qualified JS.ConnectionPen         as UI

import           Object.Object
import           Object.Port
import           Object.Node
import qualified Object.Widget.Node       as UINode
import qualified Object.Widget.Connection as UIConnection
import           Object.UITypes
import           Object.Widget

import           Event.Keyboard           hiding      (Event)
import qualified Event.Keyboard           as Keyboard
import           Event.Mouse              hiding      (Event, widget)
import qualified Event.Mouse              as Mouse
import           Event.Event
import qualified Event.ConnectionPen      as ConnectionPen

import qualified Reactive.State.Graph              as Graph
import qualified Reactive.State.Global             as Global
import           Reactive.State.Global             (State)
import qualified Reactive.State.UIRegistry         as UIRegistry

import           Reactive.Commands.Graph
import           Reactive.Commands.Command         (Command, performIO, execCommand)
import           Reactive.Commands.DisconnectNodes (disconnectAll)

import qualified Reactive.State.ConnectionPen      as ConnectionPen

import qualified BatchConnector.Commands           as BatchCmd

import           Debug.Trace

import           Control.Monad.State               hiding (State)


toAction :: Event -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton (KeyMods False True False False) _)) = Just $ startConnecting event
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton (KeyMods True  True False False) _)) = Just $ startDisconnecting event
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ whileDrawing $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just $ whileDrawing   stopDrag
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
    forM_ drawingMay $ \drawing -> run drawing

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

lookupNode :: WidgetId -> Command State (Maybe (WidgetFile State UINode.Node))
lookupNode = zoom Global.uiRegistry . UIRegistry.lookupTypedM

lookupConnection :: WidgetId -> Command State (Maybe (WidgetFile State UIConnection.Connection))
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
    when (not . null $ connections) $ disconnectAll connections

remdups               :: (Eq a) => [a] -> [a]
remdups (x : xx : xs) =  if x == xx then remdups (x : xs) else x : remdups (xx : xs)
remdups xs            = xs

zipAdj x = zip x $ tail x

maybeLast [] = Nothing
maybeLast xs = Just $ last xs

autoConnectAll :: [(Int, Int)] -> Command State ()
autoConnectAll []    = return ()
autoConnectAll nodes = autoConnectForward (head nodes) -- TODO: forall - foldr

autoConnectForward :: (Int, Int) -> Command State ()
autoConnectForward (srcNodeId, dstNodeId) = autoConnect (srcNodeId, dstNodeId)

autoConnectBackwards :: (Int, Int) -> Command State ()
autoConnectBackwards (srcNodeId, dstNodeId) = autoConnect (dstNodeId, srcNodeId)

autoConnect :: (Int, Int) -> Command State ()
autoConnect (srcNodeId, dstNodeId) = do
    graph     <- use Global.graph
    workspace <- use Global.workspace
    let srcNode          = Graph.getNode graph srcNodeId
        dstNode          = Graph.getNode graph dstNodeId
        srcPorts         = srcNode ^. ports . outputPorts
        dstPorts         = dstNode ^. ports . inputPorts
        dstPortsFiltered = filterConnectedInputPorts graph dstNodeId $ dstNode ^. ports . inputPorts
        connectionMay    = findConnectionForAll dstPortsFiltered srcPorts
    forM_ connectionMay $ \(srcPortId, dstPortId) -> do
        let srcPortRef   = PortRef srcNodeId OutputPort srcPortId
            dstPortRef   = PortRef dstNodeId InputPort  dstPortId
        connectNodes srcPortRef dstPortRef

filterConnectedInputPorts :: Graph.State -> NodeId -> PortCollection -> PortCollection
filterConnectedInputPorts state nodeId ports = filter isConnected ports where
    destinationPortRefs = fmap (^. Graph.destination) $ Graph.getConnections state
    isConnected port = not $ PortRef nodeId InputPort (port ^. portId) `elem` destinationPortRefs

findConnectionForAll :: PortCollection -> PortCollection -> Maybe (PortId, PortId)
findConnectionForAll dstPorts srcPorts = listToMaybe . catMaybes $ findConnection dstPorts <$> srcPorts

findConnection :: PortCollection -> Port -> Maybe (PortId, PortId)
findConnection dstPorts srcPort = (srcPort ^. portId,) <$> dstPortId where
    dstPortId = fmap (^. portId) $
        find (\port -> MockHelper.typesEq (port ^. portValueType) (srcPort ^. portValueType)) dstPorts
