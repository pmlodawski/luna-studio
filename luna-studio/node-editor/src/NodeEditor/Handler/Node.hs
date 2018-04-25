module NodeEditor.Handler.Node where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.ScreenPosition             (ScreenPosition)
import           LunaStudio.Data.Position                   (fromTuple)
import           NodeEditor.Action.Basic                    (collapseToFunction, enterNode, moveNode, removeSelectedNodes, selectAll,
                                                             selectNode, setNodeExpression, setPortDefault, toggleSelect, toggleSelectedNodesMode,
                                                             toggleSelectedNodesUnfold, unselectAll)
import           NodeEditor.Action.Batch                    (autolayoutNodes)
import qualified NodeEditor.Action.Node                     as Node
import qualified NodeEditor.Action.Port                     as PortControl
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes)
import           NodeEditor.Event.Event                     (Event (Shortcut, UI))
import qualified NodeEditor.Event.Mouse                     as Mouse
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.Event.UI                        (UIEvent (AppEvent, NodeEvent, SidebarEvent))
import           NodeEditor.Event.Event                     (Event (View))
import qualified NodeEditor.Event.View                      as View
import           NodeEditor.Event.View                      (BaseEvent (NodeMove, NodeSelect), ViewEvent (ViewEvent))
import qualified NodeEditor.React.Event.App                 as App
import qualified NodeEditor.React.Event.Node                as Node hiding (nodeLoc)
import qualified NodeEditor.React.Event.Sidebar             as Sidebar
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.State.Action                    (Action (continue))
import           NodeEditor.State.Global                    (State)
import           NodeEditor.State.Mouse                     (mousePosition, mousePosition', workspacePosition, workspacePosition')
import           React.Flux                                 (MouseEvent, mouseButton, mouseCtrlKey, mouseMetaKey)

handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))                            = Just $ handleCommand command
handle (UI (NodeEvent    (Node.Event nl (Node.MouseDown     mevt))))    = Just $ handleMouseDown' mevt nl
handle (UI (AppEvent     (App.MouseMove                     mevt _  ))) = Just $ handleMouseMove' mevt
handle (UI (SidebarEvent (Sidebar.MouseMove                 mevt _ _))) = Just $ handleMouseMove' mevt
handle (UI (AppEvent     (App.Movement                      move    ))) = Just $ handleMovement move
handle (UI (AppEvent     (App.MouseUp                       mevt    ))) = Just $ handleMouseUp'  mevt
handle (UI (NodeEvent    (Node.Event nl Node.Enter)))                   = Just $ withJustM (getExpressionNode nl) enterNode
handle (UI (NodeEvent    (Node.Event nl Node.EditExpression)))          = Just $ Node.editExpression nl
handle (UI (NodeEvent    (Node.Event nl Node.EditName)))                = Just $ Node.editName nl
handle (UI (NodeEvent    (Node.Event nl (Node.Select        kevt))))    = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nl
handle (UI (NodeEvent    (Node.Event nl (Node.SetExpression expr))))    = Just $ setNodeExpression nl expr
handle (UI (NodeEvent    (Node.Event nl Node.MouseEnter)))              = Just $ Node.handleMouseEnter nl
handle (UI (NodeEvent    (Node.Event nl Node.MouseLeave)))              = Just $ Node.handleMouseLeave nl
handle (View (ViewEvent _  nl (NodeMove pos))) = Just $ moveNode (convert nl) $ fromTuple pos
handle (View (ViewEvent _  nl (NodeSelect select))) = Just $ if select then selectNode $ convert nl else unselectAll

handle (View (ViewEvent path nl base)) = case path of
    ["NodeEditor", "ExpressionNode"] -> case View.type_ base of
        "dblclick"   -> Just $ withJustM (getExpressionNode $ convert nl) enterNode
        "mouseover"  -> Just $ Node.handleMouseEnter $ convert nl
        "mouseout"   -> Just $ Node.handleMouseLeave $ convert nl
        _ -> Nothing
    _ -> Nothing
handle _                                                                = Nothing

handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.AutolayoutAllNodes      -> map (view Node.nodeLoc) <$> getExpressionNodes >>= flip autolayoutNodes True
    Shortcut.AutolayoutSelectedNodes -> map (view Node.nodeLoc) <$> getSelectedNodes   >>= flip autolayoutNodes False
    Shortcut.CollapseToFunction      -> collapseToFunction
    -- Shortcut.EditSelectedNodes       -> toggleSelectedNodesMode $ Node.Expanded Node.Editor --TEMPORARILY DISABLED
    Shortcut.ExpandSelectedNodes     -> toggleSelectedNodesMode $ Node.Expanded Node.Controls
    Shortcut.RemoveSelectedNodes     -> removeSelectedNodes
    Shortcut.SelectAll               -> selectAll
    Shortcut.UnfoldSelectedNodes     -> toggleSelectedNodesUnfold
    Shortcut.Accept                  -> continue PortControl.acceptEditTextPortControl >> PortControl.unfocusEditTextPortControl
    _                                -> return ()

handleMouseDown :: BaseEvent -> NodeLoc -> Command State ()
handleMouseDown evt nl =
    when shouldProceed $ workspacePosition' evt >>= \pos -> Node.startNodeDrag pos nl shouldSnap where
        shouldProceed = View.withoutMods evt View.leftButton || View.withShift evt View.leftButton
        shouldSnap    = View.withoutMods evt View.leftButton

handleMouseMove :: BaseEvent -> Command State ()
handleMouseMove evt = if View.mouseButton View.leftButton evt
    then do
        coord <- workspacePosition' evt
        continue . Node.nodesDrag coord $ View.withoutMods evt View.leftButton
    else handleMouseUp evt

handleMouseUp :: BaseEvent -> Command State ()
handleMouseUp evt = do
    mousePosition' evt >>= continue . PortControl.stopMoveSlider
    coord <- workspacePosition' evt
    continue $ Node.handleNodeDragMouseUp coord

handleMouseDown' :: MouseEvent -> NodeLoc -> Command State ()
handleMouseDown' evt nl =
    when shouldProceed $ workspacePosition evt >>= \pos -> Node.startNodeDrag pos nl shouldSnap where
        shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
        shouldSnap    = Mouse.withoutMods evt Mouse.leftButton

handleMouseMove' :: MouseEvent -> Command State ()
handleMouseMove' evt = if mouseButton evt == Mouse.leftButton
    then do
        coord <- workspacePosition evt
        continue . Node.nodesDrag coord $ Mouse.withoutMods evt Mouse.leftButton
    else handleMouseUp' evt

handleMovement :: ScreenPosition -> Command State ()
handleMovement = continue . PortControl.moveSlider

handleMouseUp' :: MouseEvent -> Command State ()
handleMouseUp' evt = do
    mousePosition evt >>= continue . PortControl.stopMoveSlider
    coord <- workspacePosition evt
    continue $ Node.handleNodeDragMouseUp coord
