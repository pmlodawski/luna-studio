module Reactive.Plugins.Core.Action.MultiSelection where

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

import           JS.Bindings
import           JS.Appjs
import qualified JS.Utils       as Utils
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
import           Reactive.Plugins.Core.Action.State.MultiSelection
import qualified Reactive.Plugins.Core.Action.State.Selection      as Selection
import qualified Reactive.Plugins.Core.Action.State.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Global         as Global


data DragType = StartDrag
              | Moving
              | Dragging
              | StopDrag
              deriving (Eq, Show)


data Action = DragSelect { _actionType    :: DragType
                         , _startPos      :: Vector2 Int
                         }
              deriving (Eq, Show)

makeLenses ''Action


instance PrettyPrinter DragType where
    display = show

instance PrettyPrinter Action where
    display (DragSelect tpe point) = "msA( " <> display tpe <> " " <> display point <> " )"


toAction :: Event Node -> NodeCollection -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) nodes = case button of
    1                  -> case tpe of
        Mouse.Pressed  -> if isNode then Nothing
                                    else case keyMods of
                                        (KeyMods False False False False) -> Just (DragSelect StartDrag pos)
                                        _                                 -> Nothing
        Mouse.Released -> Just (DragSelect StopDrag pos)
        Mouse.Moved    -> Just (DragSelect Moving pos)
    _                  -> Nothing
    where isNode        = not . null $ nodes
toAction _ _            = Nothing


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction  newState
        where
        oldDrag                          = oldState ^. Global.multiSelection . history
        oldNodes                         = oldState ^. Global.nodes
        oldSelection                     = oldState ^. Global.selection . Selection.nodeIds
        newState                         = oldState & Global.iteration                     +~ 1
                                                    & Global.multiSelection . history      .~ newDrag
                                                    & Global.selection . Selection.nodeIds .~ newNodeIds
                                                    & Global.nodes                         .~ newNodes
        newNodes                         = updateNodesSelection newNodeIds oldNodes
        newAction                        = case newActionCandidate of
            DragSelect Moving pt        -> case oldDrag of
                Nothing                 -> Nothing
                _                       -> Just $ DragSelect Dragging pt
            _                           -> Just newActionCandidate
        newNodeIds                       = case newActionCandidate of
            DragSelect tpe point        -> case tpe of
                Moving                  -> case oldDrag of
                    Just oldDragState   -> getNodeIdsIn startPos point (Global.toCamera oldState) oldNodes
                        where startPos   = oldDragState ^. dragStartPos
                    Nothing             -> oldSelection
                _                       -> oldSelection
        newDrag                          = case newActionCandidate of
            DragSelect tpe point        -> case tpe of
                StartDrag               -> Just $ DragHistory point point
                Moving                  -> case oldDrag of
                    Just oldDragState   -> Just $ DragHistory startPos point
                        where startPos   = oldDragState ^. dragStartPos
                    Nothing             -> Nothing
                StopDrag                -> Nothing


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragSelect Dragging _  -> unselectNodes unselectedNodeIds
                               >>   selectNodes   selectedNodeIds
                               >> mapM_ setNodeFocused topNodeId
        _                      -> return ()
        where selectedNodeIds   = state ^. Global.selection . Selection.nodeIds
              unselectedNodeIds = filter (\nodeId -> not $ nodeId `elem` selectedNodeIds) $ (^. ident) <$> state ^. Global.nodes
              topNodeId         = selectedNodeIds ^? ix 0
