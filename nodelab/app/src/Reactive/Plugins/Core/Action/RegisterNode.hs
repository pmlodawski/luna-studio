{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.RegisterNode where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Utils.MockHelper as MockHelper

import           Data.Fixed

import qualified JS.Camera      as Camera

import           Object.Object
import           Object.Port
import           Object.Node    as Node
import           Object.Widget
import qualified Object.Widget.Node as WNode

import           Event.Event
import qualified Event.Keyboard     as Keyboard
import qualified Event.NodeSearcher as NodeSearcher

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global          as Global
import qualified Reactive.Plugins.Core.Action.State.Graph           as Graph
import           Reactive.Plugins.Core.Action.Commands.RegisterNode (registerNode)
import           Reactive.Plugins.Core.Action.Commands.Command      (execCommand)

import qualified Utils.MockHelper   as MockHelper
import qualified BatchConnector.Commands as BatchCmd

import           JS.NodeGraph
import           JS.Node          (createPendingNode)
import qualified Data.IntMap.Lazy as IntMap


data Action   = RegisterNodeAction Text
              | UpdateNodeAction   Text Int

data Reaction = PerformIO (IO ())
              | UpdateNodeIO Node

instance PrettyPrinter Action where
    display (RegisterNodeAction expr) = "arA(RegisterAction " <> display expr  <> ")"

instance PrettyPrinter Reaction where
    display _ = "arA(RegisterIO)"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Keyboard (Keyboard.Event Keyboard.Press 'a' _)) state = ifNoneFocused state action where
    action = Just $ RegisterNodeAction "Hello.node"
toAction (NodeSearcher (NodeSearcher.Event "create" expr Nothing   )) _  = Just $ RegisterNodeAction expr
toAction (NodeSearcher (NodeSearcher.Event "create" expr (Just nid))) _  = Just $ UpdateNodeAction   expr nid
toAction _ _  = Nothing

instance ActionStateUpdater Action where
    execSt (RegisterNodeAction expr) state = ActionUI (PerformIO action) newState where
        (action, newState) = execCommand (registerNode expr) state

    execSt (UpdateNodeAction expr nid) state = ActionUI updateNodes newState where
        updateNodes = UpdateNodeIO newNode
        graph       = state ^. Global.graph
        nodes       = Graph.getNodesMap $ graph
        newNodes    = nodes & ix nid .~ newNode
        oldNode     = nodes IntMap.! nid
        newNode     = oldNode & expression .~ expr
        newGraph    = Graph.updateNodes newNodes graph
        newState    = state & Global.graph .~ newGraph

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) _) = action

    updateUI (WithState (UpdateNodeIO node) state) = updateLabel node
                                                  >> BatchCmd.updateNodes (state ^. Global.workspace) [node]
                                                  >> BatchCmd.runMain
