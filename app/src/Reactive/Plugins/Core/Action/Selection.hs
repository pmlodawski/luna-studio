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

data State = State { _nodes :: NodeSelection
                   } deriving (Eq, Show)

type ActionState = WithStateMaybe Action State

makeLenses ''Action
makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display UnselectAll = "sa( UnselectAll )"
    display (SelectAction tpe node)  = "sa( " <> display tpe <> " " <> display node <> " )"

instance PrettyPrinter State where
    display (State nodes) = display nodes




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


accumActionState :: Action -> ActionState -> ActionState
accumActionState newAction oldActionState = WithState (Just newAction) $ State newNodes
    where
    oldAction                      = oldActionState ^. action
    oldState                       = oldActionState ^. state
    oldNodes                       = oldState ^. nodes
    newNodes                       = case newAction of
        UnselectAll               -> []
        SelectAction tpe node     -> case tpe of
            SelectNew             -> [newNode]
            Focus                 -> newNode : oldFilteredNodes
            ToggleOn              -> newNode : oldFilteredNodes
            ToggleOff             -> oldFilteredNodes
            where oldFilteredNodes = removeNode node oldNodes
                  newNode          = select node
                  removeNode      :: Node -> [Node] -> [Node]
                  removeNode       = deleteBy $ on (==) (view ident)

toNodeSelection :: ActionState -> NodeSelection
toNodeSelection =  (^. state . nodes)


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
        where selectedNodes = state ^. nodes
              topNodeId = selectedNodes ^? ix 0 . ident
