module Reactive.Plugins.Core.Action.Connect where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Debug.Trace

import           JS.Camera
import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Port
import           Object.Node
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection as UIConnection

import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects, widget )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.Executors.Graph
import           Reactive.Plugins.Core.Action.State.Connect
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor

import qualified BatchConnector.Commands as BatchCmd

import           AST.GraphToViz

data ActionType = StartDrag PortRef
                | Moving
                | Dragging Angle
                | StopDrag
                | ConnectPorts   PortRef PortRef
                | ConnectPortsUI PortRef PortRef (IO ())

data Action = DragAction { _actionType :: ActionType
                         , _actionPos  :: Vector2 Int
                         }


makeLenses ''Action


instance PrettyPrinter ActionType where
    display (StartDrag portRef)        = "StartDrag(" <> display portRef <> ")"
    display Moving                     = "Moving"
    display (Dragging angle)           = "Dragging(" <> display angle <> ")"
    display StopDrag                   = "StopDrag"
    display (ConnectPorts src dst)     = "Connect(" <> display src <> ", " <> display dst <> ")"
    display (ConnectPortsUI src dst _) = "ConnectUI(" <> display src <> ", " <> display dst <> ")"

instance PrettyPrinter Action where
    display (DragAction tpe point) = "cA(" <> display tpe <> " " <> display point <> ")"


toAction :: Event Node -> Global.State -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods _)) state = case button of
    LeftButton         -> case tpe of
        Mouse.Pressed  -> if dragAllowed then case keyMods of
                                             (KeyMods False False False False) -> Just $ DragAction (StartDrag draggedPort) pos
                                             _                                 -> Nothing
                                         else Nothing
                        where   -- TODO: switch to our RayCaster
                            portMay       = getPortRefUnderCursor state
                            dragAllowed   = isJust   portMay
                            draggedPort   = fromJust portMay
        Mouse.Released -> case (sourcePortRef, dstPortRef) of
                            (Just source, Just destination) -> Just $ DragAction (ConnectPorts source destination) pos
                            (_,           _)                -> Just $ DragAction StopDrag pos
                        where
                            sourcePortRef = state ^? Global.connect . connecting . _Just . sourcePort
                            dstPortRef    = getPortRefUnderCursor state
        Mouse.Moved    -> Just $ DragAction Moving pos
        _              -> Nothing
    _                  -> Nothing
toAction _ _            = Nothing


-- angle :: Camera.Camera -> Graph -> Maybe Connecting -> Double
calculateAngle camera oldGraph (Just (Connecting source destinationMay (DragHistory startPos currentPos))) = calcAngle destinPoint sourcePoint where
    sourcePoint = getNodePos (Graph.getNodesMap oldGraph) $ source ^. refPortNodeId
    destinPoint = screenToWorkspace camera currentPos
calculateAngle _ _ Nothing = 0.0


instance ActionStateUpdater Action where
    execSt action@(DragAction (StartDrag source) point) oldState = ActionUI action newState where
        oldGraph                             = oldState ^. Global.graph
        newState                             = oldState & Global.iteration            +~ 1
                                                        & Global.connect . connecting .~ newConnecting
                                                        & Global.graph                .~ newGraph
        newConnecting                        = Just $ Connecting source Nothing (DragHistory point point)
        newGraph                             = Graph.updateNodes newNodesMap oldGraph where
            newNodesMap                      = updateSourcePortInNodes angle source oldNodesMap
            oldNodesMap                      = Graph.getNodesMap oldGraph
            angle                            = calculateAngle camera oldGraph newConnecting
            camera                           = Global.toCamera oldState

    execSt action@(DragAction Moving point) oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        oldConnecting                        = oldState ^. Global.connect . connecting
        oldGraph                             = oldState ^. Global.graph
        newState                             = oldState & Global.iteration            +~ 1
                                                        & Global.connect . connecting .~ newConnecting
                                                        & Global.graph                .~ newGraph
        newAction                            = (DragAction (Dragging angle) point) <$ oldConnecting
        angle                                = calculateAngle camera oldGraph newConnecting
        camera                               = Global.toCamera oldState

        newConnecting                        = case oldConnecting of
            Just (Connecting source _ oldHistory)
                                            -> Just $ Connecting source Nothing newHistory where
                newHistory                   = oldHistory & dragCurrentPos .~ point
            Nothing                         -> Nothing
        newGraph                             = case newConnecting of
            Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                            -> Graph.updateNodes newNodesMap oldGraph where
                newNodesMap                  = updateSourcePortInNodes angle source oldNodesMap
                oldNodesMap                  = Graph.getNodesMap oldGraph
            _                               -> oldGraph

    execSt action@(DragAction StopDrag point) oldState = ActionUI action newState
        where
        newState                             = oldState & Global.iteration            +~ 1
                                                        & Global.connect . connecting .~ Nothing

    execSt action@(DragAction (ConnectPorts src dst) point) oldState = ActionUI (DragAction (ConnectPortsUI src dst uiUpdate) point) newState''
        where
        oldConnecting                        = oldState ^. Global.connect . connecting
        newState''                           = updateConnections $ updatePortAngles newState'
        newState'                            = newState & Global.iteration            +~ 1
                                                        & Global.connect . connecting .~ Nothing
        (uiUpdate, newState)              = case oldConnecting of
            Just (Connecting _ _ (DragHistory _ _)) -> connectNodes src dst oldState
            _                                       -> (return (), oldState)

instance ActionUIUpdater Action where
    updateUI (WithState (DragAction tpe pt) state) = case tpe of
        Dragging angle                   -> forM_ maybeConnecting $ displayDragLine nodesMap angle ptWs
        StopDrag                         -> UI.removeCurrentConnection
        ConnectPortsUI src dst uiUpdate  -> do
                                                UI.removeCurrentConnection
                                                uiUpdate
                                                updatePortAnglesUI  state
                                                updateConnectionsUI state
                                                BatchCmd.connectNodes workspace src dst
                                                putStrLn (display $ state ^. Global.graph . Graph.nodesRefsMap)   -- debug
                                                putStrLn (display $ state ^. Global.graph . Graph.connectionsMap) -- debug
                                                graphToViz (state ^. Global.graph . Graph.graphMeta)
        _                                -> return ()
        where
            nodesMap                      = Graph.getNodesMap       $ state ^. Global.graph
            connectionsMap                = Graph.getConnectionsMap $ state ^. Global.graph
            ptWs                          = screenToWorkspace camera pt
            camera                        = Global.toCamera state
            maybeConnecting               = state ^. Global.connect . connecting
            workspace                     = state ^. Global.workspace
