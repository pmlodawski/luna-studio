module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus

import           Object.Node

import           Event.Event
import qualified Event.Batch        as Batch

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Graph       as Graph
import qualified Reactive.Plugins.Core.Action.State.Global      as Global
import qualified Reactive.Plugins.Core.Action.Commands.AddNode  as AddNode
import           Reactive.Plugins.Core.Action.Commands.Command  (runCommand)

import           AST.GraphToViz

data Action = AddAction Node

data Reaction = PerformIO (IO ())

instance PrettyPrinter Action where
    display (AddAction node)          = "arA(AddAction "      <> display node  <> ")"

instance PrettyPrinter Reaction where
    display _ = "PerformIO()"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Batch (Batch.NodeAdded node)) state = Just $ AddAction node
toAction _ _ = Nothing

instance ActionStateUpdater Action where
    execSt (AddAction node) state = ActionUI (PerformIO action) newState where
        (action, newState) = runCommand (AddNode.addNode node) state

instance ActionUIUpdater Reaction where
    updateUI (WithState action state) = case action of
        PerformIO action -> action
                         >> putStrLn (display $ state ^. Global.graph . Graph.nodesRefsMap)
                         >> putStrLn (display $ state ^. Global.graph . Graph.nodesMap)
                         >> graphToViz (state ^. Global.graph . Graph.graphMeta)
