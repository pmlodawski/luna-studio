{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.AddRemove where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Utils.MockHelper as MockHelper

import           Data.Fixed

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI
import qualified JS.Camera      as Camera

import           Object.Object
import           Object.Port
import           Object.Node
import           Object.Widget
import qualified Object.Widget.Node as WNode

import           Event.Event
import           Event.Keyboard     hiding (Event)
import qualified Event.Keyboard     as Keyboard
import           Event.Mouse        hiding (Event, WithObjects, widget)
import qualified Event.Mouse        as Mouse
import qualified Event.Batch        as Batch
import           Event.NodeSearcher hiding  (Event, expression)
import qualified Event.NodeSearcher as NodeSearcher

import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.AddRemove
import qualified Reactive.Plugins.Core.Action.State.Graph       as Graph
import qualified Reactive.Plugins.Core.Action.State.Selection   as Selection
import qualified Reactive.Plugins.Core.Action.State.Global      as Global
import qualified Reactive.Plugins.Core.Action.Executors.AddNode as AddNode

import           ThreeJS.Types
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
            | AddActionUI Node (IO ())


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (AddAction node)          = "arA(AddAction "      <> display node  <> ")"
    display (BulkAdd nodes)           = "arA(BulkAdd "        <> display nodes <> ")"
    display (RegisterNodeAction expr) = "arA(RegisterAction " <> display expr  <> ")"
    display RemoveFocused             = "arA(RemoveFocused)"
    display (AddActionUI _ _)         = "arA(AddActionUI)"

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

createNode :: NodeId -> Vector2 Double -> Text -> Node
createNode nodeId pos expr = Node nodeId False pos expr $ MockHelper.createPorts expr

instance ActionStateUpdater Action where
    -- The logic of computing nodeId is needed only for offline mode
    -- once everyone has the backend running, batch will handle that
    execSt (RegisterNodeAction expr) state = ActionUI registerNode state
        where
        registerNode = RegisterActionUI node
        node         = createNode nextId nodePosWs expr
        camera       = Global.toCamera state
        graph        = state ^. Global.graph
        nextId       = Global.genId state
        nodePosWs    = Camera.screenToWorkspace camera $ state ^. Global.mousePos

    execSt (BulkAdd nodes) state = execSt (AddAction <$> nodes) state

    execSt (AddAction node) state = ActionUI (AddActionUI node action) newState where
        (newState, action) = AddNode.addNode node state

    execSt RemoveFocused oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        newState                = oldState & Global.iteration                     +~ 1
                                           & Global.graph                         .~ newGraph
                                           & Global.selection . Selection.nodeIds .~ newSelIds
                                           & Global.addRemove . toRemoveIds       .~ newToRemoveIds
        oldGraph                = oldState ^. Global.graph
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
        RegisterActionUI node -> addNode workspace node >> putStrLn ("added " <> display node)
            where
            workspace       = state ^. Global.workspace
        AddActionUI node action -> action
                                >> putStrLn (display $ state ^. Global.graph . Graph.nodesRefsMap) -- debug
                                >> putStrLn (display $ state ^. Global.graph . Graph.nodesMap)     -- debug
                                >> graphToViz (state ^. Global.graph . Graph.graphMeta)
        RemoveFocused      -> UI.removeNode nodeId
                           >> mapM_ UI.setNodeFocused topNodeId
            where
            selectedNodeIds = state ^. Global.selection . Selection.nodeIds
            nodeId          = head $ state ^. Global.addRemove . toRemoveIds
            topNodeId       = selectedNodeIds ^? ix 0
