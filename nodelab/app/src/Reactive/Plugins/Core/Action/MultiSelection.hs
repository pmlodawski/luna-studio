module Reactive.Plugins.Core.Action.MultiSelection where

import           Utils.PreludePlus
import           Utils.Vector

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI
import qualified JS.Camera      as Camera

import           Object.Object
import           Object.Node
import           Object.UITypes

import           Event.Keyboard hiding      (Event)
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.MultiSelection
import           Reactive.Plugins.Core.Action.State.UnderCursor
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection      as Selection
import qualified Reactive.Plugins.Core.Action.State.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Global         as Global

data DragType = StartDrag
              | Moving
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
    display (DragSelect tpe point) = "msA(" <> display tpe <> " " <> display point <> ")"


toAction :: Event Node -> Global.State -> UnderCursor -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods evWdgt)) state underCursor = case button of
    LeftButton   -> case tpe of
        Mouse.Pressed  -> if dragAllowed then case keyMods of
                                             (KeyMods False False False False) -> Just (DragSelect StartDrag pos)
                                             _                                 -> Nothing
                                         else Nothing
                        where   -- TODO: switch to our RayCaster
                            portMay       = getPortRefUnderCursor state
                            dragAllowed   = (null $ underCursor ^. nodesUnderCursor) && (isNothing portMay) && (isNothing evWdgt)
        Mouse.Released -> Just (DragSelect StopDrag pos)
        Mouse.Moved    -> Just (DragSelect Moving pos)
        _              -> Nothing
    _                  -> Nothing
toAction _ _  _         = Nothing


instance ActionStateUpdater Action where
    execSt action@(DragSelect StartDrag coord) state = ActionUI action newState where
        newState = state & Global.multiSelection . history ?~ (DragHistory coord coord)

    execSt action@(DragSelect Moving coord) state = ActionUI newAction newState where
        dragHistory    = state ^. Global.multiSelection . history
        nodes          = state ^. Global.graph . Graph.nodes
        selection      = state ^. Global.selection . Selection.nodeIds
        newAction      = action <$ dragHistory
        newState       = state & Global.multiSelection . history           .~ newDragHistory
                               & Global.selection      . Selection.nodeIds .~ newSelection
                               & Global.graph %~ (Graph.selectNodes newSelection)
        newDragHistory = (dragCurrentPos .~ coord) <$> dragHistory
        newSelection   = case newDragHistory of
            Just (DragHistory start current) -> getNodeIdsIn start current (Global.toCamera state) nodes
            _                                -> selection

    execSt action@(DragSelect StopDrag coord) state = ActionUI action newState where
        newState = state & Global.multiSelection . history .~ Nothing

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragSelect Moving   _  -> do
                                  UI.displaySelectionBox startSelectionBox endSelectionBox
                                  UI.unselectNodes unselectedNodeIds
                                  UI.selectNodes     selectedNodeIds
                                  mapM_ UI.setNodeFocused topNodeId
        DragSelect StopDrag _  -> UI.hideSelectionBox
        _                      -> return ()
        where selectedNodeIds   = state ^. Global.selection . Selection.nodeIds
              nodeList          = state ^. Global.graph . Graph.nodes
              unselectedNodeIds = filter (\nodeId -> not $ nodeId `elem` selectedNodeIds) $ (^. nodeId) <$> nodeList
              topNodeId         = selectedNodeIds ^? ix 0
              dragState         = fromJust (state ^. Global.multiSelection . history)
              camera            = Global.toCamera state
              currWorkspace     = Camera.screenToWorkspace camera
              startSelectionBox = currWorkspace $ dragState ^. dragStartPos
              endSelectionBox   = currWorkspace $ dragState ^. dragCurrentPos
