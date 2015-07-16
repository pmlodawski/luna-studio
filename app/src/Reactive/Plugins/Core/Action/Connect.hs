module Reactive.Plugins.Core.Action.Connect where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Function
import           System.Mem

import qualified JS.NodeGraph   as UI
import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
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


data ActionType = StartDrag
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
    display = show

instance PrettyPrinter Action where
    display (DragAction tpe point) = "cA(" <> display tpe <> " " <> display point <> ")"


toAction :: Event Node -> UnderCursor -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) underCursor = case button of
    1                  -> case tpe of
        Mouse.Pressed  -> if dragAllowed then case keyMods of
                                             (KeyMods False False False False) -> Just (DragAction StartDrag pos)
                                             _                                 -> Nothing
                                         else Nothing
        Mouse.Released -> Just (DragAction StopDrag pos)
        Mouse.Moved    -> Just (DragAction Moving   pos)
    _                  -> Nothing
    where dragAllowed   = False
toAction _ _ = Nothing



instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction  newState
        where
        oldDrag                          = oldState ^. Global.connect . history
        newState                         = oldState & Global.iteration         +~ 1
                                                    & Global.connect . history .~ newDrag
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldDrag of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction Dragging pt
            _                           -> Just newActionCandidate
        newDrag                          = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag               -> Just $ DragHistory point point
                Moving                  -> case oldDrag of
                    Just oldDragState   -> Just $ DragHistory startPos point
                        where startPos   = oldDragState ^. dragStartPos
                    Nothing             -> Nothing
                StopDrag                -> Nothing


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt            -> case tpe of
            StartDrag                -> return ()
            Moving                   -> return ()
            Dragging                 -> return ()
            StopDrag                 -> return ()


