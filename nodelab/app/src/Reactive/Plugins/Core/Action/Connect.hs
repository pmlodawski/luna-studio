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
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Object.UITypes
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Connect
import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor


data ActionType = StartDrag PortRef
                | Moving
                | Dragging Angle
                | StopDrag
                | ConnectPort PortRef
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
        Mouse.Released -> case getPortRefUnderCursor state of
                            Just draggedPort -> Just $ DragAction (ConnectPort draggedPort) pos
                            Nothing          -> Just $ DragAction StopDrag pos
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
                ConnectPort destination -> case oldConnecting of
                    Just (Connecting source _ oldHistory)
                                        -> Just $ Connecting source (Just destination) newHistory where
                        newHistory       = oldHistory & dragCurrentPos .~ point
                    Nothing             -> Nothing
                StopDrag                -> Nothing
        newGraph                         = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                ConnectPort destination -> case oldConnecting of
                    Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> Graph.updateNodes newNodes oldGraph where
                        newNodes         = updateSourcePortInNodes angle source oldNodes
                        oldNodes         = Graph.getNodes oldGraph
                    _                   -> oldGraph
                _                       -> case newConnecting of
                    Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> Graph.updateNodes newNodes oldGraph where
                        newNodes         = updateSourcePortInNodes angle source oldNodes
                        oldNodes         = Graph.getNodes oldGraph
                    _                   -> oldGraph
        angle                            = case newConnecting of
            Just (Connecting source destinationMay (DragHistory startPos currentPos))
                                        -> calcAngle destinPoint sourcePoint where
                camera                   = Global.toCamera oldState
                sourcePoint              = source ^. refPortNode . nodePos
                destinPoint              = screenToWorkspace camera currentPos
            _                           -> 0.0

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt            -> case tpe of
            StartDrag portRef        -> return ()
            Moving                   -> return ()
            Dragging angle           -> forM_ maybeConnecting $ displayDragLine angle ptWs
            StopDrag                 -> UI.removeCurrentConnection
            ConnectPort portRef      -> return ()
            where
                ptWs                  = screenToWorkspace camera pt
                camera                = Global.toCamera state
                maybeConnecting       = state ^. Global.connect . connecting


displayDragLine :: Angle -> Vector2 Double -> Connecting -> IO ()
displayDragLine angle ptWs@(Vector2 cx cy) connecting = do
    let portRef              = connecting ^. sourcePort
        ndWs@(Vector2 nx ny) = portRef ^. refPortNode . nodePos
        outerPos             = portOuterBorder + distFromPort
        sy                   = ny + outerPos * sin angle
        sx                   = nx + outerPos * cos angle
        (Vector2 vx vy)      = ptWs - ndWs
        draw                 = vx * vx + vy * vy > portOuterBorderSquared
    setAnglePortRef angle portRef
    if draw then UI.displayCurrentConnection sx sy cx cy
            else UI.removeCurrentConnection

setAnglePortRef :: Angle -> PortRef -> IO ()
setAnglePortRef refAngle portRef = setAngle (portRef ^. refPortType) refNodeId (portRef ^. refPortId) refAngle where
    refNodeId = portRef ^. refPortNode . nodeId

setAngle :: PortType -> NodeId -> PortId -> Angle -> IO ()
setAngle  InputPort = UI.setInputPortAngle
setAngle OutputPort = UI.setOutputPortAngle
