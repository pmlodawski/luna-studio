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
import           Object.Widget
import qualified Object.Widget.Node as WNode
import           Object.Widget.Scene (sceneGraphId)
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.AddNode  as AddNode
import           Event.Event
import           Event.NodeSearcher hiding  ( Event, expression )
import qualified Event.NodeSearcher as NodeSearcher
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.AddRemove
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection  as Selection
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import           Batch.Workspace
import           BatchConnector.Commands   (addNode)
import           BatchConnector.Connection (sendMessage)


import           AST.GraphToViz

data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction Node
            | RegisterNodeAction Text Workspace
            | RemoveFocused
            | AddActionUI Node WNode.Node
            | RegisterActionUI Node Workspace
            deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (AddAction node)            = "arA(AddAction "      <> display node <> ")"
    display (RegisterNodeAction expr _) = "arA(RegisterAction " <> display expr <> ")"
    display RemoveFocused               = "arA(RemoveFocused)"

toAction :: Workspace -> Event Node -> Global.State -> Maybe Action
toAction _ (AddNode (AddNode.AddNode node)) state = Just $ AddAction node
toAction workspace (Keyboard (Keyboard.Event Keyboard.Press char)) state = ifNoneFocused state $ case char of
    'a'      -> Just $ RegisterNodeAction "Hello.node" workspace
    'r'      -> Just RemoveFocused
    _        -> Nothing
toAction workspace (NodeSearcher (NodeSearcher.Event tpe expr)) _ = case tpe of
    "create" -> Just $ RegisterNodeAction expr workspace
    _        -> Nothing
toAction _ _ _  = Nothing

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
    inputPortsNum   = 1 -- tmpGetInputPortsNr  $ Text.unpack expr
    outputPortsNum  = 1 -- tmpGetOutputPortsNr $ Text.unpack expr

instance ActionStateUpdater Action where
    -- The logic of computing nodeId is needed only for offline mode
    -- once everyone has the backend running, batch will handle that
    execSt (RegisterNodeAction expr workspace) state = ActionUI registerNode state
        where
        registerNode = RegisterActionUI node workspace
        node         = createNode nextId nodePosWs expr
        camera       = Global.toCamera state
        oldNodes     = Graph.getNodes graph
        graph        = state ^. Global.graph
        nextId       = 1 + (maxNodeId oldNodes)
        nodePosWs    = Camera.screenToWorkspace camera $ state ^. Global.mousePos

    execSt (AddAction node) oldState = ActionUI newAction newState
        where
        newAction               = AddActionUI node newWNode
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.graph                         .~ newGraph
                                           & Global.uiRegistry                    .~ newRegistry
        oldGraph                = oldState ^. Global.graph
        oldRegistry             = oldState ^. Global.uiRegistry
        newGraph                = Graph.addNode node oldGraph
        (newWNode, newRegistry) = UIRegistry.register sceneGraphId widget oldRegistry where
            widget = WNode.Node def (node ^. nodeId)

    execSt RemoveFocused oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.graph                         .~ newGraph
                                           & Global.selection . Selection.nodeIds .~ newSelIds
                                           & Global.addRemove . toRemoveIds       .~ newToRemoveIds
        oldGraph                = oldState ^. Global.graph
        oldNodes                = Graph.getNodes oldGraph
        camera                  = Global.toCamera oldState
        nodePosWs               = Camera.screenToWorkspace camera $ oldState ^. Global.mousePos
        oldSelNodeIds           = oldState ^. Global.selection . Selection.nodeIds
        headNodeId              = listToMaybe oldSelNodeIds
        nextNodeId              = 1 + (maxNodeId oldNodes)
        newAction               = case headNodeId of
            Nothing        -> Nothing
            _              -> Just RemoveFocused
        newToRemoveIds          = maybeToList headNodeId
        newSelIds               = drop 1 oldSelNodeIds
        newGraph                = case headNodeId of
            Just remId     -> Graph.removeNode remId oldGraph
            Nothing        -> oldGraph

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        RegisterActionUI node workspace -> sendMessage $ addNode workspace node

        AddActionUI node wnode  -> createNodeOnUI node wnode
                                >> putStrLn (display $ state ^. Global.graph) -- debug
                                >> graphToViz (state ^. Global.graph . Graph.graphMeta)
        RemoveFocused      -> UI.removeNode nodeId
                           >> mapM_ UI.setNodeFocused topNodeId
            where
            selectedNodeIds = state ^. Global.selection . Selection.nodeIds
            nodeId          = head $ state ^. Global.addRemove . toRemoveIds
            topNodeId       = selectedNodeIds ^? ix 0



createNodeOnUI :: Node -> WNode.Node -> IO ()
createNodeOnUI node wnode = do
    let
        pos        = node ^. nodePos
        ident      = node ^. nodeId
        expr       = node ^. expression
    UI.createNodeAt ident pos expr (objectId wnode)
    addPorts  InputPort node
    addPorts OutputPort node


addPorts :: PortType -> Node -> IO ()
addPorts portType node = mapM_ (addPortWith (node ^. nodeId) $ addPort portType) $ getPorts portType node

addPortWith :: NodeId -> (NodeId -> PortId -> Double -> IO ()) -> Port -> IO ()
addPortWith nodeId addPortFun port = addPortFun nodeId (port ^. portId) (port ^. angle)

addPort :: PortType -> NodeId -> PortId -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
