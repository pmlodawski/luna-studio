{-# LANGUAGE TupleSections #-}

module Reactive.Plugins.Core.Action.ConnectionPen where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified Data.IntMap.Lazy as IntMap

import qualified JS.Bindings        as UI
import qualified JS.ConnectionPen   as UI

import           Object.Object
import           Object.Port
import           Object.Node
import qualified Object.Widget.Node as UINode
import qualified Object.Widget.Connection as UIConnection
import           Object.UITypes
import           Object.Widget

import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects, widget )
import qualified Event.Mouse    as Mouse
import           Event.Event
import qualified Event.ConnectionPen as ConnectionPen
import           Event.WithObjects

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.Executors.Graph
import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Global        as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry    as UIRegistry
import qualified Reactive.Plugins.Core.Action.Executors.AddNode   as AddNode
import qualified Reactive.Plugins.Core.Action.State.ConnectionPen as ConnectionPen

import qualified BatchConnector.Commands as BatchCmd

import Debug.Trace


data Action = BeginDrawing  (Vector2 Int) ConnectionPen.DrawingType
            | NextPoint     (Vector2 Int)
            | NextPointData [NodeId] [ConnectionId]
            | FinishDrawing (Vector2 Int)
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display v = "gCP(" <> show v <> ")"

data Reaction = PerformIO (IO ())
              | NoOp

instance PrettyPrinter Reaction where
    display _ = "ConnectionPenReaction"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos LeftButton keyMods _)) state = case tpe of
    Mouse.Pressed  -> case keyMods of
        (KeyMods False True False False)  -> Just (BeginDrawing pos ConnectionPen.Connecting)
        (KeyMods True  True False False)  -> Just (BeginDrawing pos ConnectionPen.Disconnecting)
        _                                 -> Nothing
    Mouse.Released -> if isDrawing then Just (FinishDrawing pos) else Nothing
    Mouse.Moved    -> if isDrawing then Just (NextPoint     pos) else Nothing
    _              -> Nothing
    where isDrawing = isJust $ state ^. Global.connectionPen . ConnectionPen.drawing
toAction (ConnectionPen (ConnectionPen.Segment widgets)) state = case state ^. Global.connectionPen . ConnectionPen.drawing of
    Just drawing                     -> case drawing ^. ConnectionPen.drawingType of
        ConnectionPen.Connecting     -> pushEvent where
            pushEvent                 = if null nodes then Nothing
                                                      else Just $ NextPointData nodes []
            nodes                     = catMaybes $ fmap (^. widget . UINode.nodeId) <$> lookupNode <$> widgets
            lookupNode :: WidgetId   -> Maybe (WidgetFile Global.State UINode.Node)
            lookupNode widgetId       = UIRegistry.lookupTyped widgetId registry
        ConnectionPen.Disconnecting  -> pushEvent where
            pushEvent                 = if null connections then Nothing
                                                            else Just $ NextPointData [] connections
            connections               = catMaybes $ fmap (^. widget . UIConnection.connectionId) <$> lookupConnection <$> widgets
            lookupConnection :: WidgetId -> Maybe (WidgetFile Global.State UIConnection.Connection)
            lookupConnection widgetId = UIRegistry.lookupTyped widgetId registry
        where
        registry                      = state ^. Global.uiRegistry

    Nothing -> Nothing

toAction _ _ = Nothing

remdups               :: (Eq a) => [a] -> [a]
remdups (x : xx : xs) =  if x == xx then remdups (x : xs) else x : remdups (xx : xs)
remdups xs            = xs

zipAdj x = zip x $ tail x

maybeLast [] = Nothing
maybeLast xs = Just $ last xs

instance ActionStateUpdater Action where
    execSt (BeginDrawing pos tpe) state = ActionUI (PerformIO draw) newState where
        draw     = UI.beginPath pos (tpe == ConnectionPen.Connecting)
        newState = state & Global.connectionPen . ConnectionPen.drawing .~ Just (ConnectionPen.Drawing pos tpe Nothing [])

    execSt (NextPoint pos) state = ActionUI (PerformIO requestNodesBetween) newState where
        newState        = state  & Global.connectionPen . ConnectionPen.drawing .~ Just newPen
        (Just oldPen)   = state ^. Global.connectionPen . ConnectionPen.drawing
        newPen          = oldPen & ConnectionPen.previousPos  .~ pos

        requestNodesBetween = do
            UI.clearCanvas
            UI.drawSegment pos
            UI.requestWidgetsBetween ((fromJust $ state ^. Global.connectionPen . ConnectionPen.drawing) ^. ConnectionPen.previousPos) pos


    execSt (NextPointData nodes connections) state = case (fromJust $ state ^. Global.connectionPen . ConnectionPen.drawing) ^. ConnectionPen.drawingType of
        ConnectionPen.Connecting -> ActionUI (PerformIO draw) newState' where
            newState          = state  & Global.connectionPen . ConnectionPen.drawing .~ Just newPen
            (Just oldPen)     = state ^. Global.connectionPen . ConnectionPen.drawing
            pos               = oldPen ^. ConnectionPen.previousPos
            newPen            = oldPen & ConnectionPen.visitedNodes %~ (++ nodes)
                                       & ConnectionPen.lastNode     .~ (maybeLast path)
            path              = remdups $ (maybeToList $ oldPen ^. ConnectionPen.lastNode) ++ nodes
            nodesToConnect    = zipAdj path
            (draw, newState') = if null nodesToConnect then (return (), newState)
                                                       else (fullUI, st) where
                                                            (ui, st) = autoConnectAll nodesToConnect newState
                                                            fullUI   = do ui
                                                                          moveNodesUI $ getNodesMap $ state ^. Global.graph
                                                                          updatePortAnglesUI st
                                                                          updateConnectionsUI st
                                                                          putStrLn $ "connectNodes " <> show nodesToConnect


                -- TODO: connecting nodes UI actions
        ConnectionPen.Disconnecting -> ActionUI (PerformIO draw) newState'' where
            newState''      = updateConnections $ updatePortAngles newState'
            newState'       = state & Global.graph      %~ removeConnections connections
                                    & Global.uiRegistry %~ UIRegistry.unregisterAll_ widgetIds
            (Just oldPen)   = state ^. Global.connectionPen . ConnectionPen.drawing
            pos             = oldPen ^. ConnectionPen.previousPos
            widgetIds       = connectionToWidgetId connections (state ^. Global.uiRegistry)
            draw            = do  -- TODO: remove from UIRegistry
                                  updateConnectionsUI newState'
                                  UI.removeConnections widgetIds
                                  putStrLn $ "disconnecting " <> show connections

    execSt (FinishDrawing _) state = ActionUI (PerformIO draw) newState where
        newState      = state  & Global.connectionPen . ConnectionPen.drawing .~ Nothing
        draw          = UI.endPath


connectionToWidgetId :: [ConnectionId] -> UIRegistry.State Global.State -> [WidgetId]
connectionToWidgetId connections state = widgetIds where
    widgetIds = catMaybes $ ((flip IntMap.lookup) idsMap) <$> connections
    idsMap    = IntMap.fromList $ (\w -> (w ^. widget . UIConnection.connectionId, w ^. objectId)) <$> widgets
    widgets   = UIRegistry.lookupAll state :: [WidgetFile Global.State UIConnection.Connection]


autoConnectAll :: [(Int, Int)] -> Global.State -> (IO (), Global.State)
autoConnectAll nodes = autoConnect $ head nodes -- TODO: forall - foldr

autoConnect :: (Int, Int) -> Global.State -> (IO (), Global.State)
autoConnect (srcNodeId, dstNodeId) oldState = (uiUpdate, newState') where
    graph                            = oldState ^. Global.graph
    workspace                        = oldState ^. Global.workspace
    srcNode                          = getNode graph srcNodeId
    dstNode                          = getNode graph dstNodeId
    srcPorts                         = srcNode ^. ports . outputPorts
    dstPorts                         = dstNode ^. ports . inputPorts
    -- dstPorts                         = filterConnectedInputPorts graph dstNodeId $ dstNode ^. ports . inputPorts
    connection                       = findConnectionForAll dstPorts srcPorts
    newState'                        = updateConnections $ updatePortAngles newState
    (uiUpdate, newState)             = case connection of
        Just (srcPortId, dstPortId) -> (do
                                            connectUI
                                            BatchCmd.connectNodes workspace srcPortRef dstPortRef
                                        , st)
                                       where
            (connectUI, st)          = connectNodes srcPortRef dstPortRef oldState
            srcPortRef               = PortRef srcNodeId OutputPort srcPortId
            dstPortRef               = PortRef dstNodeId InputPort  dstPortId
        Nothing                     -> (return (), oldState)

filterConnectedInputPorts :: State -> NodeId -> PortCollection -> PortCollection
filterConnectedInputPorts state nodeId ports = filter isConnected ports where
    destinationPortRefs = fmap (^. destination) $ getConnections state
    isConnected port = PortRef nodeId InputPort (port ^. portId) `elem` destinationPortRefs

findConnectionForAll :: PortCollection -> PortCollection -> Maybe (PortId, PortId)
findConnectionForAll dstPorts srcPorts = listToMaybe . catMaybes $ findConnection dstPorts <$> srcPorts

findConnection :: PortCollection -> Port -> Maybe (PortId, PortId)
findConnection dstPorts srcPort = (srcPort ^. portId,) <$> dstPortId where
    dstPortId = fmap (^. portId) $ find (\port -> port ^. portType == srcPort ^. portType) dstPorts


instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) state) = do
                                                        action
                                                        moveNodesUI $ getNodesMap $ state ^. Global.graph
                                                        updatePortAnglesUI state -- TODO: does not work...
                                                        updateConnectionsUI state
    updateUI (WithState NoOp state)               = return ()
