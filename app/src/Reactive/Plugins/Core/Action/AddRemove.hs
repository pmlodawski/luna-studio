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
import           JS.Utils       as Utils

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.AddRemove
import qualified Reactive.Plugins.Core.Action.State.Selection as Selection
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction
            | RemoveFocused
            deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display AddAction     = "arA(AddAction)"
    display RemoveFocused = "arA(RemoveFocused)"



toAction :: Event Node -> Maybe Action
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    'a'   -> Just AddAction
    'r'   -> Just RemoveFocused
    _     -> Nothing
toAction _ = Nothing

maxNodeId :: NodeCollection -> NodeId
maxNodeId []    = 0
maxNodeId nodes = (view ident) $ maximumBy (on compare (view ident)) nodes

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction  newState
        where
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.nodes                         .~ newNodes
                                           & Global.selection . Selection.nodeIds .~ newSelIds
                                           & Global.addRemove . toRemoveIds       .~ newToRemoveIds
        oldNodes                = oldState ^. Global.nodes
        camera                  = Global.toCamera oldState
        nodePosWs               = Utils.screenToWorkspace camera $ oldState ^. Global.mousePos
        oldSelNodeIds           = oldState ^. Global.selection . Selection.nodeIds
        headNodeId              = listToMaybe oldSelNodeIds
        nextNodeId              = 1 + (maxNodeId oldNodes)
        newAction               = case newActionCandidate of
            RemoveFocused      -> case headNodeId of
                Nothing        -> Nothing
                _              -> Just newActionCandidate
            _                  -> Just newActionCandidate
        newToRemoveIds          = case newAction of
            Just RemoveFocused -> maybeToList headNodeId
            _                  -> []
        newSelIds               = case newAction of
            Just RemoveFocused -> drop 1 oldSelNodeIds
            _                  -> oldSelNodeIds
        newNodes                = case newActionCandidate of
            AddAction          -> (Node nextNodeId False nodePosWs) : oldNodes
            RemoveFocused      -> case headNodeId of
                Nothing        -> oldNodes
                Just remId     -> filter (\node -> node ^. ident /= remId) oldNodes

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        AddAction          -> newNodeAt nodeId px py
            where
            node            = head $ state ^. Global.nodes
            (Vector2 px py) = node ^. Node.position
            nodeId          = node ^. ident
        RemoveFocused      -> removeNode nodeId
                           >> mapM_ setNodeFocused topNodeId
            where
            selectedNodeIds = state ^. Global.selection . Selection.nodeIds
            nodeId          = head $ state ^. Global.addRemove . toRemoveIds
            topNodeId       = selectedNodeIds ^? ix 0
