module NodeEditor.Handler.Node where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Position                   (fromTuple)
import           NodeEditor.Action.Basic                    (collapseToFunction, enterNode, moveNode, removeSelectedNodes, selectAll,
                                                             selectNode, toggleSelectedNodesMode,
                                                             toggleSelectedNodesUnfold, unselectAll)
import           NodeEditor.Action.Batch                    (autolayoutNodes)
import qualified NodeEditor.Action.Port                     as PortControl
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import           NodeEditor.Event.Event                     (Event (View))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.Event.View                      (ViewEvent)
import qualified NodeEditor.Event.View                      as View
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node hiding (nodeLoc')
import           NodeEditor.State.Action                    (Action (continue))
import           NodeEditor.State.Global                    (State)


nodeEventPath :: View.Path
nodeEventPath = ["NodeEditor", "ExpressionNode"]

handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))  = handleCommand command
handle (View evt)                             = handleViewEvent evt
handle _ = Nothing


handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = let nl = View.getNodeLoc evt
    in case evt ^. View.base of
        View.NodeMove   e -> Just . moveNode nl . fromTuple $ e ^. View.position
        View.NodeSelect e -> Just
            $ if e ^. View.select then selectNode nl else unselectAll
        View.Mouse e -> if nodeEventPath /= evt ^. View.path
            then Nothing
            else case e ^. View.type_ of
                "dblclick"
                    -> Just $ withJustM (getExpressionNode nl) enterNode
                _   -> Nothing
        _ -> Nothing


handleCommand :: Shortcut.Command -> Maybe (Command State ())
handleCommand = \case
    Shortcut.SelectAll           -> Just selectAll
    Shortcut.CollapseToFunction  -> Just collapseToFunction
    Shortcut.RemoveSelectedNodes -> Just removeSelectedNodes
    Shortcut.UnfoldSelectedNodes -> Just toggleSelectedNodesUnfold
    Shortcut.ExpandSelectedNodes -> Just
        . toggleSelectedNodesMode $ Node.Expanded Node.Controls
    Shortcut.Accept -> Just $ continue PortControl.acceptEditTextPortControl
    Shortcut.AutolayoutAllNodes
        -> Just $ map (view Node.nodeLoc) <$> getExpressionNodes
            >>= flip autolayoutNodes True
    Shortcut.AutolayoutSelectedNodes
        -> Just $ map (view Node.nodeLoc) <$> getSelectedNodes
            >>= flip autolayoutNodes False
    _   -> Nothing
    -- Shortcut.EditSelectedNodes       -> toggleSelectedNodesMode $ Node.Expanded Node.Editor --TEMPORARILY DISABLED
