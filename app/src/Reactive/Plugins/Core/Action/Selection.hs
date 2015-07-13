module Reactive.Plugins.Core.Action.Selection where

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

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Selection
import qualified Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global

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


toAction :: Event Node -> NodeCollection -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) nodes = case button of
    1                 -> case tpe of
        Mouse.Pressed -> if isNoNode then case keyMods of
                                        (KeyMods False False False False) -> Just UnselectAll
                                        _                                 -> Nothing
                                     else case keyMods of
                                        (KeyMods False False False False) -> Just (SelectAction selectActionType node)
                                        (KeyMods False False True  False) -> Just (SelectAction toggleActionType node)
                                        _                                 -> Nothing
        _             -> Nothing
    _                 -> Nothing
    where isNoNode         = null nodes
          node             = head nodes
          selectActionType = if node ^. selected then Focus
                                                 else SelectNew
          toggleActionType = if node ^. selected then ToggleOff
                                                 else ToggleOn
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) state = case char of
    'A'     -> Just SelectAll
    _       -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down  char)) state = case char of
    '\27'   -> Just UnselectAll
    _       -> Nothing
toAction _ _ = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction newState
        where
        oldNodeIds                       = oldState ^. Global.selection . nodeIds
        oldNodes                         = oldState ^. Global.nodes
        newNodes                         = updateNodesSelection newNodeIds oldNodes
        newState                         = oldState & Global.iteration           +~ 1
                                                    & Global.selection . nodeIds .~ newNodeIds
                                                    & Global.nodes               .~ newNodes
        newNodeIds                       = case newAction of
            SelectAll                   -> (^. ident) <$> oldNodes
            UnselectAll                 -> []
            SelectAction tpe node       -> case tpe of
                SelectNew               -> [newNodeId]
                Focus                   -> newNodeId : oldFilteredNodeIds
                ToggleOn                -> newNodeId : oldFilteredNodeIds
                ToggleOff               -> oldFilteredNodeIds
                where oldFilteredNodeIds = delete newNodeId oldNodeIds
                      newNodeId          = node ^. ident


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        SelectAction tpe (Node nodeId _ _ _) -> case tpe of
            SelectNew         -> unselectAllNodes
                              >> setNodeFocused nodeId
            Focus             -> setNodeFocused nodeId
            ToggleOn          -> setNodeFocused nodeId
            ToggleOff         -> setNodeUnselected nodeId
                              >> mapM_ setNodeFocused topNodeId
        SelectAll             -> selectAllNodes
                              >> mapM_ setNodeFocused topNodeId
        UnselectAll           -> unselectAllNodes
        where selectedNodeIds  = state ^. Global.selection . nodeIds
              topNodeId        = selectedNodeIds ^? ix 0
