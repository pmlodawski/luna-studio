module Reactive.Plugins.Core.Action.Connect where

import           Utils.PreludePlus
import           Utils.Vector
import           Debug.Trace

import           JS.Camera
import qualified JS.NodeGraph   as UI
import           Object.Object
import           Object.Port
import           Object.Node
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Connect
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor


data ActionType = StartDrag PortRef
                | Moving
                | Dragging Angle
                | StopDrag
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


toAction :: Event Node -> UnderCursor -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) underCursor = trace ("uc " <> display underCursor) $ case button of
    1                  -> case tpe of
        Mouse.Pressed  -> if dragAllowed then case keyMods of
                                             (KeyMods False False False False) -> Just (DragAction (StartDrag draggedPort) pos)
                                             _                                 -> Nothing
                                         else Nothing
        Mouse.Released -> Just (DragAction StopDrag pos)
        Mouse.Moved    -> Just (DragAction Moving   pos)
    _                  -> Nothing
    where dragAllowed   = isJust   $ underCursor ^. port
          draggedPort   = fromJust $ underCursor ^. port
toAction _ _            = Nothing

updateSourcePort :: PortRef -> Angle -> Node -> Node
updateSourcePort portRef angle node = if node ^. nodeId == portRef ^. refPortNode . nodeId then newNode else node where
    newNode = updatePortAngle portRef angle node

updateSourcePortInNodes :: Angle -> PortRef -> NodeCollection -> NodeCollection
updateSourcePortInNodes angle portRef nodes = updateSourcePort portRef angle <$> nodes

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        oldConnecting                    = oldState ^. Global.connect . connecting
        oldNodes                         = oldState ^. Global.nodes
        newState                         = oldState & Global.iteration            +~ 1
                                                    & Global.connect . connecting .~ newConnecting
                                                    & Global.nodes                .~ newNodes
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldConnecting of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction (Dragging angle) pt
            _                           -> Just newActionCandidate
        newConnecting                    = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag source        -> Just $ Connecting source (DragHistory point point)
                Moving                  -> case oldConnecting of
                    Just (Connecting source oldHistory)
                                        -> Just $ Connecting source newHistory where
                        newHistory       = oldHistory & dragCurrentPos .~ point
                    Nothing             -> Nothing
                StopDrag                -> Nothing
        newNodes                         = case newConnecting of
            Just (Connecting source (DragHistory startPos currentPos))
                                        -> updateSourcePortInNodes angle source oldNodes where
            _                           -> oldNodes
        angle                            = case newConnecting of
            Just (Connecting source (DragHistory startPos currentPos))
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
            Dragging angle           -> forM_ maybeSourcePort $ setAnglePortRef angle
            StopDrag                 -> return ()
        where
            maybeSourcePort           = (^. sourcePort) <$> state ^. Global.connect . connecting


setAnglePortRef :: Angle -> PortRef -> IO ()
setAnglePortRef refAngle portRef = setAngle (portRef ^. refPortType) refNodeId (portRef ^. refPortId) refAngle where
    refNodeId = portRef ^. refPortNode . nodeId

setAngle :: PortType -> NodeId -> PortId -> Angle -> IO ()
setAngle  InputPort = UI.setInputPortAngle
setAngle OutputPort = UI.setOutputPortAngle
