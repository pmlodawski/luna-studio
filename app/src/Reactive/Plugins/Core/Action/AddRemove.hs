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
import qualified Reactive.Plugins.Core.Action.State.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.State.Selection as Selection
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction
            | RemoveFocused
            deriving (Eq, Show)


-- type ActionState = WithStateMaybe Action Global.State

makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display AddAction     = "arA( AddAction )"
    display RemoveFocused = "arA( RemoveFocused )"



toAction :: Event Node -> Maybe Action
toAction (Keyboard (Keyboard.Event char)) = case char of
    'a'   -> Just AddAction
    'r'   -> Just RemoveFocused
    _     -> Nothing
toAction _ = Nothing

maxNodeId :: NodeCollection -> NodeId
maxNodeId []    = 0
maxNodeId nodes = (view ident) $ maximumBy (on compare (view ident)) nodes

instance ActionStateExecutor Action Global.State where
    exec newActionCandidate oldState = WithState newAction newState
        where
        newState                = oldState & Global.iteration +~ 1
                                           & Global.nodes .~ newNodes
                                           & Global.selection . Selection.nodeIds .~ newSelIds
                                           & Global.addRemove . AddRemove.toRemoveIds .~ toRemoveIds
        oldNodes                = oldState ^. Global.nodes
        nodePos                 = oldState ^. Global.mousePos
        oldSelNodeIds           = oldState ^. Global.selection . Selection.nodeIds
        headNodeId              = listToMaybe oldSelNodeIds
        nextNodeId              = 1 + (maxNodeId oldNodes)
        newAction               = case newActionCandidate of
            RemoveFocused      -> case headNodeId of
                Nothing        -> Nothing
                _              -> Just newActionCandidate
            _                  -> Just newActionCandidate
        toRemoveIds             = case newAction of
            Just RemoveFocused -> maybeToList headNodeId
            _                  -> []
        newSelIds               = case newAction of
            Just RemoveFocused -> drop 1 oldSelNodeIds
            _                  -> oldSelNodeIds
        newNodes                = case newActionCandidate of
            AddAction          -> (Node nextNodeId False nodePos) : oldNodes
            RemoveFocused      -> case headNodeId of
                Nothing        -> oldNodes
                Just remId     -> filter (\node -> node ^. ident /= remId) oldNodes


updateUI :: WithStateMaybe Action Global.State -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing               -> return ()
    Just AddAction        -> newNodeAt nodeId px py
        where
        node   = head $ state ^. Global.nodes
        px = node ^. Node.position . x
        py = node ^. Node.position . y
        nodeId = node ^. ident
    Just RemoveFocused    -> removeNode nodeId
                          >> mapM_ setNodeFocused topNodeId
        where
        selectedNodeIds = state ^. Global.selection . Selection.nodeIds
        nodeId    = head $ state ^. Global.addRemove . AddRemove.toRemoveIds
        topNodeId = selectedNodeIds ^? ix 0


-- instance SuperAction (WithStateMaybe Action Global.State) where
--     trans :: Global.State -> Maybe AddRemove.Action -> WithStateMaybe Action Global.State
--     trans st
--     upUI  :: WithStateMaybe Action Global.State -> IO ()
--     upUI = updateUI
