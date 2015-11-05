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

import           Reactive.Plugins.Core.Action
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
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction (ConnectionPen (ConnectionPen.Segment widgets))                     = Just $ handleAction widgets
toAction _                                                                   = Nothing

startConnecting :: Mouse.Event -> Command State ()
startConnecting event@(Mouse.Event _ coord _ _ _) = do
    performIO $ UI.beginPath coord True
    Global.connectionPen . ConnectionPen.drawing .= Just (ConnectionPen.Drawing coord ConnectionPen.Connecting Nothing [])

startDisconnecting :: Mouse.Event -> Command State ()
startDisconnecting event@(Mouse.Event _ coord _ _ _) = do
    performIO $ UI.beginPath coord False
    Global.connectionPen . ConnectionPen.drawing .= Just (ConnectionPen.Drawing coord ConnectionPen.Disconnecting Nothing [])

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    drawingMay <- use $ Global.connectionPen . ConnectionPen.drawing
    forM_ drawingMay $ \drawing -> do
        let previousPos = drawing ^. ConnectionPen.previousPos
        Global.connectionPen . ConnectionPen.drawing %= (fmap $ ConnectionPen.previousPos .~ coord)
        performIO $ do
            UI.clearCanvas
            UI.drawSegment coord
            UI.requestWidgetsBetween previousPos coord

stopDrag :: Command State ()
stopDrag = do
    drawingMay <- use $ Global.connectionPen . ConnectionPen.drawing
    forM_ drawingMay $ \drawing -> do
        Global.connectionPen . ConnectionPen.drawing .= Nothing
        performIO UI.endPath

lookupNode :: WidgetId -> Command State (Maybe (WidgetFile State UINode.Node))
lookupNode = zoom Global.uiRegistry . UIRegistry.lookupTypedM

lookupConnection :: WidgetId -> Command State (Maybe (WidgetFile State UIConnection.Connection))
lookupConnection = zoom Global.uiRegistry . UIRegistry.lookupTypedM

handleAction :: [WidgetId] -> Command State ()
handleAction widgets = do
    drawingMay <- use $ Global.connectionPen . ConnectionPen.drawing
    forM_ drawingMay $ \drawing -> do
        let drawingType = drawing ^. ConnectionPen.drawingType
        registry <- use Global.uiRegistry
        case drawingType of
            ConnectionPen.Connecting -> do
                nodesMay <- sequence $ lookupNode <$> widgets
                let nodes = (view $ widget . UINode.nodeId) <$> catMaybes nodesMay
                when (not . null $ nodes) $ do
                    let path              = remdups $ (maybeToList $ drawing ^. ConnectionPen.lastNode) ++ nodes
                    Global.connectionPen . ConnectionPen.drawing %= (fmap $ ConnectionPen.visitedNodes %~ (++ nodes))
                    Global.connectionPen . ConnectionPen.drawing %= (fmap $ ConnectionPen.lastNode .~ (maybeLast path))
                    let nodesToConnect    = zipAdj path
                    state <- get
                    let (ui, newState) = autoConnectAll nodesToConnect state
                    put newState
                    performIO ui
            ConnectionPen.Disconnecting -> do
                connectionsMay <- sequence $ lookupConnection <$> widgets
                let connections = (view $ widget . UIConnection.connectionId) <$> catMaybes connectionsMay
                when (not . null $ connections) $ disconnectAll connections

remdups               :: (Eq a) => [a] -> [a]
remdups (x : xx : xs) =  if x == xx then remdups (x : xs) else x : remdups (xx : xs)
remdups xs            = xs

zipAdj x = zip x $ tail x

maybeLast [] = Nothing
maybeLast xs = Just $ last xs

autoConnectAll :: [(Int, Int)] -> Global.State -> (IO (), Global.State)
autoConnectAll []    state = (return (), state)
autoConnectAll nodes state = autoConnect (head nodes) state -- TODO: forall - foldr

autoConnect :: (Int, Int) -> Global.State -> (IO (), Global.State)
autoConnect (srcNodeId, dstNodeId) oldState =
    fromMaybe (return (), oldState) $ tryAutoConnect (srcNodeId, dstNodeId) oldState

autoConnectBackwards :: (Int, Int) -> Global.State -> (IO (), Global.State)
autoConnectBackwards (srcNodeId, dstNodeId) oldState =
    fromMaybe (return (), oldState) $ tryAutoConnect (dstNodeId, srcNodeId) oldState

tryAutoConnect :: (Int, Int) -> Global.State -> Maybe (IO (), Global.State)
tryAutoConnect (srcNodeId, dstNodeId) oldState = result where
    graph                            = oldState ^. Global.graph
    workspace                        = oldState ^. Global.workspace
    srcNode                          = Graph.getNode graph srcNodeId
    dstNode                          = Graph.getNode graph dstNodeId
    srcPorts                         = srcNode ^. ports . outputPorts
    dstPorts                         = dstNode ^. ports . inputPorts
    dstPortsFiltered                 = filterConnectedInputPorts graph dstNodeId $ dstNode ^. ports . inputPorts
    connection                       = findConnectionForAll dstPortsFiltered srcPorts
    result                           = case connection of
        Just (srcPortId, dstPortId) -> Just (do
                                            putStrLn $ "connections      " <> (display $ Graph.getConnections graph)
                                            putStrLn $ "srcPorts         " <> display srcPorts
                                            putStrLn $ "dstPorts         " <> display dstPorts
                                            putStrLn $ "dstPortsFiltered " <> display dstPortsFiltered
                                            connectUI
                                            -- MOCK TC -- BatchCmd.connectNodes workspace srcPortRef dstPortRef
                                        , snd $ execCommand (updatePortAngles >> updateConnections) st)
                                       where
            (connectUI, st)          = execCommand (connectNodes srcPortRef dstPortRef) oldState
            srcPortRef               = PortRef srcNodeId OutputPort srcPortId
            dstPortRef               = PortRef dstNodeId InputPort  dstPortId
        Nothing                     -> Nothing


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
