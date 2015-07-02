module Reactive.Plugins.Core.Action.Selection where

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
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Selection
import qualified Reactive.Plugins.Core.Action.State.Global   as Global

data ActionType = SelectNew
                | Focus
                | ToggleOn
                | ToggleOff
                deriving (Eq, Show)

data Action = SelectAction { _actionType :: ActionType
                           , _actionNode :: Node
                           }
            | UnselectAll
            deriving (Eq, Show)

type ActionState = WithStateMaybe Action Global.State

makeLenses ''Action

instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display UnselectAll = "sA( UnselectAll )"
    display (SelectAction tpe node)  = "sA( " <> display tpe <> " " <> display node <> " )"


keyboardToAction :: Keyboard.Event -> Maybe Action
keyboardToAction event = case event ^. char of
    'u' -> Just UnselectAll
    _   -> Nothing

mouseToAction :: Mouse.WithObjects Node -> Maybe Action
mouseToAction eventWithObjects = case mouseEvent ^. tpe of
    Mouse.Pressed -> if isNoNode then case mouseKeyMods of
                                    (KeyMods False False False False) -> Just UnselectAll
                                    _                                 -> Nothing
                                 else case mouseKeyMods of
                                    (KeyMods False False False False) -> Just (SelectAction selectActionType node)
                                    (KeyMods False False True  False) -> Just (SelectAction toggleActionType node)
                                    _                                 -> Nothing
    _             -> Nothing
    where mouseEvent       = eventWithObjects ^. event
          mouseKeyMods     = mouseEvent ^. keyMods
          isNoNode         = null $ eventWithObjects ^. objects
          node             = unwrap . head $ eventWithObjects ^. objects
          selectActionType = if node ^. selected then Focus
                                                 else SelectNew
          toggleActionType = if node ^. selected then ToggleOff
                                                 else ToggleOn


updateNodeSelection :: NodeIdCollection -> Node -> Node
updateNodeSelection selNodeIds node = let selection = elem (node ^. ident) selNodeIds in node & selected .~ selection

updateNodesSelection :: NodeIdCollection -> NodeCollection -> NodeCollection
updateNodesSelection selNodeIds nodes = fmap (updateNodeSelection selNodeIds) nodes


instance ActionStateExecutor Action Global.State where
    exec newAction oldState = WithState (Just newAction) newState
        where
        oldNodeIds                       = oldState ^. Global.selection . nodeIds
        oldNodes                         = oldState ^. Global.nodes
        newNodes                         = updateNodesSelection newNodeIds oldNodes
        newState                         = oldState & Global.selection .~ (State newNodeIds)
                                                  & Global.nodes     .~ newNodes
        newNodeIds                       = case newAction of
            UnselectAll                 -> []
            SelectAction tpe node       -> case tpe of
                SelectNew               -> [newNodeId]
                Focus                   -> newNodeId : oldFilteredNodeIds
                ToggleOn                -> newNodeId : oldFilteredNodeIds
                ToggleOff               -> oldFilteredNodeIds
                where oldFilteredNodeIds = delete newNodeId oldNodeIds
                      newNodeId          = node ^. ident

-- toNodeIdSelection :: ActionState -> NodeIdCollection
-- toNodeIdSelection =  (^. state . Global.selection . nodeIds)


updateUI :: ActionState -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing           -> return ()
    Just action       -> case action of
        SelectAction tpe (Node nodeId _ _) -> case tpe of
            SelectNew -> unselectAllNodes
                      >> setNodeFocused nodeId
            Focus     -> setNodeFocused nodeId
            ToggleOn  -> setNodeFocused nodeId
            ToggleOff -> setNodeUnselected nodeId
                      >> mapM_ setNodeFocused topNodeId
        UnselectAll   -> unselectAllNodes
        where selectedNodeIds = state ^. Global.selection . nodeIds
              topNodeId = selectedNodeIds ^? ix 0
