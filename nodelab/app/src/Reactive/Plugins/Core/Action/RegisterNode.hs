{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.RegisterNode where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Utils.MockHelper as MockHelper

import           Data.Fixed

import qualified JS.Camera      as Camera

import           Object.Object
import           Object.Port
import           Object.Node
import           Object.Widget
import qualified Object.Widget.Node as WNode

import           Event.Event
import qualified Event.Keyboard     as Keyboard
import qualified Event.NodeSearcher as NodeSearcher

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global      as Global

import qualified BatchConnector.Commands as BatchCmd

data Action = RegisterNodeAction Text

data Reaction = RegisterNodeIO Node

instance PrettyPrinter Action where
    display (RegisterNodeAction expr) = "arA(RegisterAction " <> display expr  <> ")"

instance PrettyPrinter Reaction where
    display (RegisterNodeIO node) = "arA(RegisterIO " <> display node  <> ")"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Keyboard (Keyboard.Event Keyboard.Press 'a')) state = ifNoneFocused state action where
    action = Just $ RegisterNodeAction "Hello.node"
toAction (NodeSearcher (NodeSearcher.Event "create" expr)) _  = Just $ RegisterNodeAction expr
toAction _ _  = Nothing

createNode :: NodeId -> Vector2 Double -> Text -> Node
createNode nodeId pos expr = Node nodeId False pos expr $ createPorts expr

instance ActionStateUpdater Action where
    execSt (RegisterNodeAction expr) state = ActionUI registerNode state where
        registerNode = RegisterNodeIO node
        node         = createNode nextId nodePosWs expr
        camera       = Global.toCamera state
        nextId       = Global.genNodeId state
        nodePosWs    = Camera.screenToWorkspace camera $ state ^. Global.mousePos

instance ActionUIUpdater Reaction where
    updateUI (WithState (RegisterNodeIO node) state) = BatchCmd.addNode workspace node where
        workspace = state ^. Global.workspace
