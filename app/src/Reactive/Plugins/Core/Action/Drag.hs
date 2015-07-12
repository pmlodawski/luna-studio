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
import           JS.Appjs
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
import qualified Reactive.Plugins.Core.Action.State.Camera   as Camera
import           Reactive.Plugins.Core.Action.State.Drag     as Drag
import qualified Reactive.Plugins.Core.Action.State.Global   as Global


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
    display (DragAction tpe point) = "dA( " <> display tpe <> " " <> display point <> " )"


toAction :: Event Node -> Global.State -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) state = case button of
    1                  -> case tpe of
        Mouse.Pressed  -> if isNoNode then Nothing
                                      else case keyMods of
                                           (KeyMods False False False False) -> Just (DragAction StartDrag pos)
                                           _                                 -> Nothing
        Mouse.Released -> Just (DragAction StopDrag pos)
        Mouse.Moved    -> Just (DragAction Moving   pos)
    _                  -> Nothing
    where objects          = getNodesAt pos (state ^. Global.camera . Camera.camera . Camera.factor) (state ^. Global.nodes)
          isNoNode         = null objects
toAction _ _ = Nothing

moveNodes :: Vector2 Int -> NodeCollection -> NodeCollection
moveNodes delta = fmap $ \node -> if node ^. selected then node & Node.position +~ delta else node


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction newState
        where
        oldDrag                          = oldState ^. Global.drag . Drag.history
        oldNodes                         = oldState ^. Global.nodes
        emptySelection                   = null oldNodes
        newState                         = oldState & Global.iteration +~ 1
                                                    & Global.drag  . Drag.history .~ newDrag
                                                    & Global.nodes .~ newNodes
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldDrag of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction Dragging pt
            _                           -> Just newActionCandidate
        newNodes                         = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag               -> oldNodes
                Moving                  -> case oldDrag of
                    Just oldDragState   -> moveNodes delta oldNodes
                        where prevPos    = oldDragState ^. dragCurrentPos
                              delta      = point - prevPos
                    Nothing             -> oldNodes
                StopDrag                -> oldNodes
        newDrag                          = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag               -> Just $ DragHistory point point point
                Moving                  -> if emptySelection then Nothing else case oldDrag of
                    Just oldDragState   -> Just $ DragHistory startPos prevPos point
                        where startPos   = oldDragState ^. dragStartPos
                              prevPos    = oldDragState ^. dragCurrentPos
                    Nothing             -> Nothing
                StopDrag                -> Nothing


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt   -> case tpe of
            StartDrag       -> return ()
            Moving          -> return ()
            Dragging        -> moveNodesUI selectedNodes
            StopDrag        -> return ()
        where selectedNodes  = state ^. Global.nodes
              topNodeId      = selectedNodes ^? ix 0 . ident


moveNodeUI :: Node -> IO ()
moveNodeUI (Node ident _ pos) = dragNode ident pos

moveNodesUI :: NodeCollection -> IO ()
moveNodesUI nodes  = mapM_ moveNodeUI nodes
                  -- >> performGC
