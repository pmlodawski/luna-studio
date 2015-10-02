module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node
import           Event.Event
import           Batch.Workspace
import qualified Event.Batch as Batch

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.Commands.Graph   as Executor
import           Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import qualified Reactive.Plugins.Core.Action.Commands.AddNode as AddNode
import           Reactive.Plugins.Core.Action.Commands.Command (runCommand)

import qualified BatchConnector.Commands as BatchCmd

data Action = AddSingleNode Node
            | AddSingleEdge (PortRef, PortRef)
            | AddNodes      [Node]
            | AddEdges      [(PortRef, PortRef)]
            | DisplayEdges
            | RequestRun
            | PrepareValues [Node]
            | GraphFetched  [Node] [(PortRef, PortRef)]
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display (GraphFetched nodes edges) = "gf(" <> display nodes <> "," <> display edges <> ")"

data Reaction = PerformIO (IO ())
              | NoOp

instance PrettyPrinter Reaction where
    display _ = "GraphFetcherReaction"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Batch (Batch.GraphViewFetched nodes edges)) state = case state ^. Global.graph . Graph.nodes of
    [] -> Just $ GraphFetched nodes edges
    _  -> Nothing
toAction _ _ = Nothing

instance ActionStateUpdater Action where
    execSt (AddSingleEdge (src, dst)) state = ActionUI (PerformIO draw) newState where
      (draw, newState) = Executor.connectNodes src dst state

    execSt (AddSingleNode node) state = ActionUI (PerformIO addAction) newState where
        (addAction, newState) = runCommand (AddNode.addNode node) state

    execSt (PrepareValues nodes) state = ActionUI (PerformIO prepareValues) state where
        workspace     = state ^. Global.workspace
        prepareValues = case workspace ^. interpreterState of
            Fresh  -> BatchCmd.insertSerializationModes workspace nodes
            AllSet -> BatchCmd.requestValues workspace nodes

    execSt DisplayEdges state = ActionUI (PerformIO draw) newState where
        draw        = Executor.updatePortAnglesUI  newState
                   >> Executor.updateConnectionsUI newState
        newState    = Executor.updateConnections $ Executor.updatePortAngles state


    execSt (AddNodes nodes) state = execSt (AddSingleNode <$> nodes) state

    execSt (AddEdges edges) state = execSt (AddSingleEdge <$> edges) state

    execSt RequestRun state = ActionUI (PerformIO BatchCmd.runMain) state

    execSt (GraphFetched nodes edges) state = execSt [AddNodes nodes, AddEdges edges, DisplayEdges, PrepareValues nodes, RequestRun] state

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) _) = action
    updateUI (WithState NoOp _)               = return ()
