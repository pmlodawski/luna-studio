module Reactive.Plugins.Core.Action.Selection where

import           Utils.PreludePlus

import           JS.Bindings
import qualified JS.NodeGraph   as UI

import           Object.Node
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Object.UITypes
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Selection
import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import           Reactive.Plugins.Core.Action.State.UnderCursor


data ActionType = SelectNew
                | Focus
                | ToggleOn
                | ToggleOff
                deriving (Eq, Show)

data Action = SelectAction { _actionType :: ActionType
                           , _actionNode :: Node
                           }
            | SelectAll
            | UnselectAll
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display UnselectAll              = "sA(UnselectAll)"
    display SelectAll                = "sA(SelectAll)"
    display (SelectAction tpe node)  = "sA(" <> display tpe <> " " <> display node <> ")"


toAction :: Event Node -> Global.State -> UnderCursor -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods _)) _ underCursor = case button of
    LeftButton  -> case tpe of
        Mouse.Pressed -> if nodeUnderCursor then case keyMods of
                                        (KeyMods False False False False) -> Just (SelectAction selectActionType node)
                                        (KeyMods False False True  False) -> Just (SelectAction toggleActionType node)
                                        _                                 -> Nothing
                                     else case keyMods of
                                        (KeyMods False False False False) -> Just UnselectAll
                                        _                                 -> Nothing
        _             -> Nothing
    _                 -> Nothing
    where nodeUnderCursor  = not . null $ underCursor ^. nodesUnderCursor
          node             = head       $ underCursor ^. nodesUnderCursor
          selectActionType = if node ^. selected then Focus
                                                 else SelectNew
          toggleActionType = if node ^. selected then ToggleOff
                                                 else ToggleOn
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) state _ = ifNoneFocused state $ case char of
    'A'     -> Just SelectAll
    _       -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down  char)) state _ = ifNoneFocused state $ case char of
    '\27'   -> Just UnselectAll
    _       -> Nothing
toAction _ _ _ = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction newState
        where
        oldNodeIds                       = oldState ^. Global.selection . nodeIds
        oldGraph                         = oldState ^. Global.graph
        oldNodes                         = Graph.getNodes oldGraph
        newGraph                         = Graph.selectNodes newNodeIds oldGraph
        newState                         = oldState & Global.iteration           +~ 1
                                                    & Global.selection . nodeIds .~ newNodeIds
                                                    & Global.graph               .~ newGraph
        newNodeIds                       = case newAction of
            SelectAll                   -> (^. nodeId) <$> oldNodes
            UnselectAll                 -> []
            SelectAction tpe node       -> case tpe of
                SelectNew               -> [newNodeId]
                Focus                   -> newNodeId : oldFilteredNodeIds
                ToggleOn                -> newNodeId : oldFilteredNodeIds
                ToggleOff               -> oldFilteredNodeIds
                where oldFilteredNodeIds = delete newNodeId oldNodeIds
                      newNodeId          = node ^. nodeId


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        SelectAction tpe node -> let selNodeId = node ^. nodeId in case tpe of
            SelectNew         -> UI.unselectAllNodes
                              >> UI.setNodeFocused selNodeId
            Focus             -> UI.setNodeFocused selNodeId
            ToggleOn          -> UI.setNodeFocused selNodeId
            ToggleOff         -> UI.setNodeUnselected selNodeId
                              >> mapM_ UI.setNodeFocused topNodeId
        SelectAll             -> UI.selectAllNodes
                              >> mapM_ UI.setNodeFocused topNodeId
        UnselectAll           -> UI.unselectAllNodes
        where selectedNodeIds  = state ^. Global.selection . nodeIds
              topNodeId        = selectedNodeIds ^? ix 0
