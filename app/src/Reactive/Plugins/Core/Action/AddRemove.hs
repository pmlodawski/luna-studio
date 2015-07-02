module Reactive.Plugins.Core.Action.AddRemove where

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

data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction
            | RemoveAction { _actionNode :: Node }
            deriving (Eq, Show)

data State = State { _nodes :: NodeCollection
                   } deriving (Eq, Show)

type ActionState = WithStateMaybe Action State

makeLenses ''Action
makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display AddAction            = "arA( AddAction )"
    display (RemoveAction node)  = "arA( RemoveAction " <> display node <> " )"

instance PrettyPrinter State where
    display (State nodes) = display nodes




keyboardToAction :: Keyboard.Event -> Maybe Action
keyboardToAction event = case event ^. char of
    'a' -> Just AddAction
    _   -> Nothing


instance ActionStateExecutor Action State where
  exec newAction oldState = WithState (Just newAction) $ State newNodes
      where
      oldNodes              = oldState ^. nodes
      newNodes              = case newAction of
          AddAction         -> (Node 777 False $ Point 100 100) : oldNodes
          RemoveAction node -> delete node oldNodes

toNodes :: ActionState -> NodeCollection
toNodes =  (^. state . nodes)


updateUI :: ActionState -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing               -> return ()
    Just AddAction        -> newNodeAt
    Just (RemoveAction _) -> return ()

    -- case action of
    --     SelectAction tpe (Node nodeId _ _) -> case tpe of
    --         SelectNew -> unselectAllNodes
    --                   >> setNodeFocused nodeId
    --         Focus     -> setNodeFocused nodeId
    --         ToggleOn  -> setNodeFocused nodeId
    --         ToggleOff -> setNodeUnselected nodeId
    --                   >> mapM_ setNodeFocused topNodeId
    --     UnselectAll   -> unselectAllNodes
    --     where selectedNodeIds = state ^. nodeIds
    --           topNodeId = selectedNodeIds ^? ix 0
