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
import qualified Reactive.Plugins.Core.Action.State.Global   as Global


data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction
            | RemoveAction { _actionNode :: Node }
            deriving (Eq, Show)


type ActionState = WithStateMaybe Action Global.State

makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display AddAction            = "arA( AddAction )"
    display (RemoveAction node)  = "arA( RemoveAction " <> display node <> " )"



keyboardToAction :: Keyboard.Event -> Maybe Action
keyboardToAction event = case event ^. char of
    'a' -> Just AddAction
    _   -> Nothing


maxNodeId :: NodeCollection -> NodeId
maxNodeId nodes = (view ident) $ maximumBy (on compare (view ident)) nodes

instance ActionStateExecutor Action Global.State where
    exec newAction oldState = WithState (Just newAction) newState
        where
        newState              = oldState & Global.nodes .~ newNodes
        oldNodes              = oldState ^. Global.nodes
        nextNodeId            = 1 + (maxNodeId oldNodes)
        newNodes              = case newAction of
            AddAction         -> (Node nextNodeId False $ Point 100 100) : oldNodes
            RemoveAction node -> delete node oldNodes

-- toNodes :: ActionState -> NodeCollection
-- toNodes =  (^. state . nodes)


updateUI :: ActionState -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing               -> return ()
    Just AddAction        -> newNodeAt
    Just (RemoveAction _) -> return ()
