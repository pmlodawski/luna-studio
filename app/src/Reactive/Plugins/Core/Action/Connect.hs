module Reactive.Plugins.Core.Action.Connect where

import           Utils.PreludePlus
import           Debug.Trace

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
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Connect
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor


data ActionType = StartDrag PortRef
                | Moving
                | Dragging
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



instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        oldConnecting                    = oldState ^. Global.connect . connecting
        newState                         = oldState & Global.iteration            +~ 1
                                                    & Global.connect . connecting .~ newConnecting
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldConnecting of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction Dragging pt
            _                           -> Just newActionCandidate
        newConnecting                    = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag source        -> Just $ Connecting source (DragHistory point point)
                Moving                  -> case oldConnecting of
                    Just (Connecting source oldHistory)
                                        -> Just $ Connecting source (DragHistory startPos point)
                        where startPos   = oldHistory ^. dragStartPos
                    Nothing             -> Nothing
                StopDrag                -> Nothing


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt            -> case tpe of
            StartDrag portRef        -> return ()
            Moving                   -> return ()
            Dragging                 -> print "dupa"
                                     >> return ()
            StopDrag                 -> return ()



setAngle :: PortType -> NodeId -> PortId -> Double -> IO ()
setAngle  InputPort = UI.setInputPortAngle
setAngle OutputPort = UI.setOutputPortAngle
