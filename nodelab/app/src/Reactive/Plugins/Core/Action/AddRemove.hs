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
import           Event.Mouse    hiding      ( Event, WithObjects, widget )
import qualified Event.Mouse    as Mouse
import qualified Event.Batch    as Batch
import           Event.Event
import           Event.NodeSearcher hiding  ( Event, expression )
import qualified Event.NodeSearcher as NodeSearcher
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.AddRemove
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection  as Selection
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import qualified ThreeJS.Registry                              as JSRegistry
import qualified ThreeJS.Scene                                 as Scene
import qualified Control.Monad.State                           as MState

import           Object.Widget.Slider                              (Slider(..))
import qualified Object.Widget.Slider                            as Slider
import           ThreeJS.Types
import qualified ThreeJS.Registry                                as JSRegistry
import qualified ThreeJS.Widget.Button                           as UIButton
import qualified ThreeJS.Widget.Slider                           as UISlider
import qualified ThreeJS.Widget.Toggle                           as UIToggle
import qualified ThreeJS.Widget.Number                           as UINumber
import qualified ThreeJS.Widget.Node                             as UINode

import           Object.Widget.Scene (sceneInterfaceId, sceneGraphId)

import           BatchConnector.Commands   (addNode)

import           AST.GraphToViz

data ActionType = Add
                | Remove
                deriving (Eq, Show)

data Action = AddAction Node
            | BulkAdd [Node]
            | RegisterNodeAction Text
            | RemoveFocused
            | RegisterActionUI Node
            | AddActionUI Node WNode.Node [Maybe (IO ())]


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (AddAction node)          = "arA(AddAction "      <> display node  <> ")"
    display (BulkAdd nodes)           = "arA(BulkAdd "        <> display nodes <> ")"
    display (RegisterNodeAction expr) = "arA(RegisterAction " <> display expr  <> ")"
    display RemoveFocused             = "arA(RemoveFocused)"
    display (AddActionUI _ _ _)       = "arA(AddActionUI)"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Batch (Batch.NodeAdded node)) state = Just $ AddAction node
toAction (Batch (Batch.GraphViewFetched nodes)) state = Just $ BulkAdd nodes
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) state = ifNoneFocused state $ case char of
    'a'      -> Just $ RegisterNodeAction "Hello.node"
    'r'      -> Just RemoveFocused
    _        -> Nothing
toAction (NodeSearcher (NodeSearcher.Event tpe expr)) _ = case tpe of
    "create" -> Just $ RegisterNodeAction expr
    _        -> Nothing
toAction _ _  = Nothing

addWidget b = do
    w <- JSRegistry.build (b ^. objectId) (b ^. widget)
    JSRegistry.register (b ^. objectId) w
    Scene.scene `add` w

addWidgetToNode node b = do
    node   <- JSRegistry.lookup node :: IO UINode.Node
    widget <- JSRegistry.build (b ^. objectId) (b ^. widget)
    JSRegistry.register (b ^. objectId) widget
    node `add` widget

-- mock helper functions
tmpMaxin  = 9
tmpMaxOut = 5
tmpGetInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (tmpMaxin + 1)
tmpGetOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` tmpMaxOut
-- end of mock

createNode :: NodeId -> Vector2 Double -> Text -> Node
createNode nodeId pos expr = Node nodeId False pos expr (createPorts inputPortsNum) where
    -- mock port numbers:
    inputPortsNum   = 1 -- tmpGetInputPortsNr  $ Text.unpack expr

instance ActionStateUpdater Action where
    -- The logic of computing nodeId is needed only for offline mode
    -- once everyone has the backend running, batch will handle that
    execSt (RegisterNodeAction expr) state = ActionUI registerNode state
        where
        registerNode = RegisterActionUI node
        node         = createNode nextId nodePosWs expr
        camera       = Global.toCamera state
        oldNodes     = Graph.getNodes graph
        graph        = state ^. Global.graph
        nextId       = Global.genId state
        nodePosWs    = Camera.screenToWorkspace camera $ state ^. Global.mousePos

    execSt (BulkAdd nodes) state = execSt (AddAction <$> nodes) state

    execSt (AddAction node) oldState = ActionUI newAction newState
        where
        newAction               = AddActionUI node (newWNode ^. widget) actions
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.graph                         .~ newGraph
                                           & Global.uiRegistry                    .~ newRegistry
        oldGraph                = oldState ^. Global.graph
        oldRegistry             = oldState ^. Global.uiRegistry
        newGraph                = Graph.addNode node oldGraph
        (newWNode, (newRegistry, actions)) = MState.runState registerWidgets (oldRegistry, [])
        registerWidgets         = do
            file <- UIRegistry.registerM sceneGraphId (WNode.Node (node ^. nodeId) []) def
            UIRegistry.uiAction $ createNodeOnUI node file

            let rootId = file ^. objectId

            slider_a <- UIRegistry.registerM rootId (Slider (Vector2 10  75) (Vector2 180 25) "Cutoff"     100.0        25000.0      0.4 :: Slider Double) def
            UIRegistry.uiAction $ addWidgetToNode rootId slider_a

            slider_b <- UIRegistry.registerM rootId (Slider (Vector2 10 105) (Vector2 180 25) "Resonance"  0.0        1.0      0.2 :: Slider Double) def
            UIRegistry.uiAction $ addWidgetToNode rootId slider_b

            slider_c <- UIRegistry.registerM rootId (Slider (Vector2 10 135) (Vector2 180 25) "Amount"     0.0        1.0      0.6 :: Slider Double) def
            UIRegistry.uiAction $ addWidgetToNode rootId slider_c

            slider_d <- UIRegistry.registerM rootId (Slider (Vector2 10 165) (Vector2 180 25) "Gain"       0.0        2.0      0.5 :: Slider Double) def
            UIRegistry.uiAction $ addWidgetToNode rootId slider_d

            UIRegistry.updateM (file ^. objectId) ((file ^. widget) & WNode.controls .~ ((^. objectId) <$> [slider_a, slider_b, slider_c, slider_d]))

            return file

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
        RegisterActionUI node -> addNode workspace node
            where
            workspace       = state ^. Global.workspace
        AddActionUI node wnode actions  -> (sequence_ $ reverse $ catMaybes actions)
                                        >> putStrLn (display $ state ^. Global.graph . Graph.nodesRefsMap) -- debug
                                        >> graphToViz (state ^. Global.graph . Graph.graphMeta)
        RemoveFocused      -> UI.removeNode nodeId
                           >> mapM_ UI.setNodeFocused topNodeId
            where
            selectedNodeIds = state ^. Global.selection . Selection.nodeIds
            nodeId          = head $ state ^. Global.addRemove . toRemoveIds
            topNodeId       = selectedNodeIds ^? ix 0



createNodeOnUI :: Node -> WidgetFile s WNode.Node -> IO ()
createNodeOnUI node file = do
    let
        pos        = node ^. nodePos
        ident      = node ^. nodeId
        expr       = node ^. expression
    UI.createNodeAt ident pos expr (file ^. objectId)
    addPorts  InputPort node
    addPorts OutputPort node


addPorts :: PortType -> Node -> IO ()
addPorts portType node = mapM_ (addPortWith (node ^. nodeId) $ addPort portType) $ getPorts portType node

addPortWith :: NodeId -> (NodeId -> PortId -> Double -> IO ()) -> Port -> IO ()
addPortWith nodeId addPortFun port = addPortFun nodeId (port ^. portId) (port ^. angle)

addPort :: PortType -> NodeId -> PortId -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
