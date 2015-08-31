{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.AddRemove where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.Fixed
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import           Debug.Trace

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI
import qualified JS.Camera      as Camera

import           Object.Object
import           Object.Port
import           Object.Node
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.NodeSearcher hiding  ( Event, expression )
import qualified Event.NodeSearcher as NodeSearcher
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.AddRemove
import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection as Selection
import qualified Reactive.Plugins.Core.Action.State.Global    as Global



data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction Text
            | RemoveFocused
            deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (AddAction expr) = "arA(AddAction " <> display expr <> ")"
    display RemoveFocused    = "arA(RemoveFocused)"


toAction :: Event Node -> Maybe Action
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    'a'      -> Just $ AddAction "Hello.node"
    'r'      -> Just RemoveFocused
    _        -> Nothing
toAction (NodeSearcher (NodeSearcher.Event tpe expr)) = case tpe of
    "create" -> Just $ AddAction expr
    _        -> Nothing
toAction _    = Nothing

maxNodeId :: NodeCollection -> NodeId
maxNodeId []    = 0
maxNodeId nodes = (^. nodeId) $ maximumBy (on compare (^. nodeId)) nodes

-- mock helper functions
tmpMaxin  = 9
tmpMaxOut = 5
tmpGetInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (tmpMaxin + 1)
tmpGetOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` tmpMaxOut
-- end of mock

createNode :: NodeId -> Vector2 Double -> Text -> Node
createNode nodeId pos expr = Node nodeId False pos expr (createPorts inputPortsNum outputPortsNum) where
    -- mock port numbers:
    inputPortsNum   = tmpGetInputPortsNr  $ Text.unpack expr
    outputPortsNum  = tmpGetOutputPortsNr $ Text.unpack expr


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.graph                         .~ newGraph
                                           & Global.selection . Selection.nodeIds .~ newSelIds
                                           & Global.addRemove . toRemoveIds       .~ newToRemoveIds
        oldGraph                = oldState ^. Global.graph
        oldNodes                = Graph.getNodes oldGraph
        newGraph                = Graph.updateNodes newNodes oldGraph
        camera                  = Global.toCamera oldState
        nodePosWs               = Camera.screenToWorkspace camera $ oldState ^. Global.mousePos
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
            AddAction expr     -> newNode : oldNodes where
                newNode         = createNode nextNodeId nodePosWs expr
            RemoveFocused      -> case headNodeId of
                Nothing        -> oldNodes
                Just remId     -> filter (\node -> node ^. nodeId /= remId) oldNodes

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        AddAction expr     -> createNodeOnUI node
            where
            node            = head . Graph.getNodes $ state ^. Global.graph
        RemoveFocused      -> UI.removeNode nodeId
                           >> mapM_ UI.setNodeFocused topNodeId
            where
            selectedNodeIds = state ^. Global.selection . Selection.nodeIds
            nodeId          = head $ state ^. Global.addRemove . toRemoveIds
            topNodeId       = selectedNodeIds ^? ix 0



createNodeOnUI :: Node ->  IO ()
createNodeOnUI node = do
    let
        pos        = node ^. nodePos
        ident      = node ^. nodeId
        expr       = node ^. expression
    UI.createNodeAt ident pos expr
    addPorts  InputPort node
    addPorts OutputPort node


addPorts :: PortType -> Node -> IO ()
addPorts portType node = mapM_ (addPortWith (node ^. nodeId) $ addPort portType) $ getPorts portType node

addPortWith :: NodeId -> (NodeId -> PortId -> Double -> IO ()) -> Port -> IO ()
addPortWith nodeId addPortFun port = addPortFun nodeId (port ^. portId) (port ^. angle)

addPort :: PortType -> NodeId -> PortId -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
