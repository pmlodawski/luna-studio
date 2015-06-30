module Reactive.Plugins.Core.Action.Drag where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Function
import           System.Mem

import           JS.Bindings
import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action


data ActionType = StartDrag
                | Moving
                | Dragging
                | StopDrag
                deriving (Eq, Show)

data DragState = DragState { _dragStartPos    :: Point
                           , _dragPreviousPos :: Point
                           , _dragCurrentPos  :: Point
                           } deriving (Eq, Show)

data Action = DragAction   { _actionType :: ActionType
                           , _actionPos  :: Point
                           }
            deriving (Eq, Show)

data State = State { _drag  :: Maybe DragState
                   , _nodes :: NodeSelection
                   } deriving (Eq, Show)


type ActionState = WithStateMaybe Action State

data AccumInput = AccumInput NodeSelection Action

makeLenses ''Action
makeLenses ''State
makeLenses ''DragState


instance Default State where
    def = State def def

instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (DragAction   tpe point) = "da( " <> display tpe <> " " <> display point <> " )"

instance PrettyPrinter State where
    display (State dragging nodes) = display dragging <> " " <> display nodes

instance PrettyPrinter DragState where
    display (DragState start prev current) = "d( " <> display start <> " " <> display prev <> " " <> display current <> " )"


mouseToAction :: Mouse.WithObjects Node -> Maybe Action
mouseToAction eventWithObjects = case mouseEvent ^. tpe of
    Mouse.Pressed  -> if isNoNode then Nothing
                                  else case mouseKeyMods of
                                       (KeyMods False False False False) -> Just (DragAction StartDrag mousePosition)
                                       _                                 -> Nothing
    Mouse.Released -> Just (DragAction StopDrag mousePosition)
    Mouse.Moved    -> Just (DragAction Moving   mousePosition)
    where mouseEvent    = eventWithObjects ^. event
          mouseKeyMods  = mouseEvent ^. keyMods
          mousePosition = mouseEvent ^. position
          isNoNode      = null $ eventWithObjects ^. objects


mergeWith :: NodeSelection -> NodeSelection -> NodeSelection
mergeWith current new = (getCommon current new) <> (getNew new current)
    where
    getNew, getCommon :: NodeSelection -> NodeSelection -> NodeSelection
    getNew    = deleteFirstsBy $ on (==) (view ident)
    getCommon = intersectBy $ on (==) (view ident)

moveNodes :: Point -> NodeSelection -> NodeSelection
moveNodes delta = fmap $ Node.position +~ delta

accumActionState :: AccumInput -> ActionState -> ActionState
accumActionState (AccumInput nodeSelection newActionCandidate) oldActionState = WithState maybeNewAction $ State newDrag newNodes
    where
    oldAction                        = oldActionState ^. action
    oldState                         = oldActionState ^. state
    oldDrag                          = oldState ^. drag
    oldNodes                         = oldState ^. nodes
    mergedOldNodes                   = oldNodes `mergeWith` nodeSelection
    emptySelection                   = null mergedOldNodes
    maybeNewAction                   = case newActionCandidate of
        DragAction Moving pt        -> case oldDrag of
            Nothing                 -> Nothing
            _                       -> Just $ DragAction Dragging pt
        _                           -> Just newActionCandidate
    newNodes                         = case newActionCandidate of
        DragAction tpe point        -> case tpe of
            StartDrag               -> mergedOldNodes
            Moving                  -> case oldDrag of
                Just oldDragState   -> moveNodes delta mergedOldNodes
                    where prevPos    = oldDragState ^. dragCurrentPos
                          delta      = point - prevPos
                Nothing             -> mergedOldNodes
            StopDrag                -> mergedOldNodes
    newDrag                          = case newActionCandidate of
        DragAction tpe point        -> case tpe of
            StartDrag               -> Just $ DragState point point point
            Moving                  -> if emptySelection then Nothing else case oldDrag of
                Just oldDragState   -> Just $ DragState startPos prevPos point
                    where startPos   = oldDragState ^. dragStartPos
                          prevPos    = oldDragState ^. dragCurrentPos
                Nothing             -> Nothing
            StopDrag                -> Nothing

updateUI :: ActionState -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing           -> return ()
    Just action       -> case action of
        DragAction tpe pt -> case tpe of
            StartDrag -> return ()
            Moving    -> return ()
            Dragging  -> moveNodesUI selectedNodes
            StopDrag  -> return ()
        where selectedNodes = state ^. nodes
              topNodeId = selectedNodes ^? ix 0 . ident


moveNodeUI :: Node -> IO ()
moveNodeUI (Node ident _ (Point x y)) = dragNode ident x y

moveNodesUI :: NodeSelection -> IO ()
moveNodesUI nodes  = mapM_ moveNodeUI nodes
                  -- >> performGC
