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

import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.Common
import           Reactive.Plugins.Core.Action.State.Connect
import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor

import qualified BatchConnector.Commands as BatchCmd

import           AST.GraphToViz

data ActionType = StartDrag PortRef
                | Moving
                | Dragging Angle
                | StopDrag
                | ConnectPorts PortRef PortRef
                deriving (Eq, Show)

data Action = DragAction { _actionType :: ActionType
                         , _actionPos  :: Vector2 Int
                         }
              deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter ActionType where
    display (StartDrag portRef) = "StartDrag(" <> display portRef <> ")"
    display other               = show other

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

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        oldConnecting                    = oldState ^. Global.connect . connecting
        oldGraph                         = oldState ^. Global.graph
        newState                         = oldState & Global.iteration            +~ 1
                                                    & Global.connect . connecting .~ newConnecting
                                                    & Global.graph                .~ newGraph
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldConnecting of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction (Dragging angle) pt
            _                           -> Just newActionCandidate
        newConnecting                    = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag source        -> Just $ Connecting source Nothing (DragHistory point point)
                Moving                  -> case oldConnecting of
                    Just (Connecting source _ oldHistory)
                                        -> Just $ Connecting source Nothing newHistory where
                        newHistory       = oldHistory & dragCurrentPos .~ point
                    Nothing             -> Nothing
                ConnectPorts _ _        -> Nothing
                StopDrag                -> Nothing
        newGraph                         = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                ConnectPorts src dst    -> case oldConnecting of
                    Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> appGraph where
                        newNodesMap      = updateSourcePortInNodes angle source oldNodesMap
                        oldNodesMap      = Graph.getNodesMap oldGraph
                        updSourceGraph   = Graph.updateNodes newNodesMap oldGraph
                        appGraph         = Graph.addConnection source dst updSourceGraph
                    _                   -> oldGraph
                _                       -> case newConnecting of
                    Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> Graph.updateNodes newNodesMap oldGraph where
                        newNodesMap      = updateSourcePortInNodes angle source oldNodesMap
                        oldNodesMap      = Graph.getNodesMap oldGraph
                    _                   -> oldGraph
        angle                            = case newConnecting of
            Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> calcAngle destinPoint sourcePoint where
                camera                   = Global.toCamera oldState
                sourcePoint              = getNodePos (Graph.getNodesMap oldGraph) $ source ^. refPortNodeId
                destinPoint              = screenToWorkspace camera currentPos
            _                           -> 0.0

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt            -> case tpe of
            StartDrag portRef        -> return ()
            Moving                   -> return ()
            Dragging angle           -> forM_ maybeConnecting $ displayDragLine nodesMap angle ptWs
            StopDrag                 -> UI.removeCurrentConnection
            ConnectPorts src dst     -> UI.removeCurrentConnection
                                     >> displayConnections nodesMap connections
                                     >> BatchCmd.connectNodes workspace src dst
                                     >> putStrLn (display $ state ^. Global.graph . Graph.nodesRefsMap) -- debug
                                     >> putStrLn (display $ state ^. Global.graph . Graph.connections) -- debug
                                     >> graphToViz (state ^. Global.graph . Graph.graphMeta)
            where
                nodesMap              = Graph.getNodesMap    $ state ^. Global.graph
                connections           = Graph.getConnections $ state ^. Global.graph
                ptWs                  = screenToWorkspace camera pt
                camera                = Global.toCamera state
                maybeConnecting       = state ^. Global.connect . connecting
                workspace             = state ^. Global.workspace
