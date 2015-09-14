module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node
import           Event.Event
import qualified Event.Batch as Batch

import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.Common            (displayConnections)
import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Global      as Global
import qualified Reactive.Plugins.Core.Action.Executors.AddNode as AddNode

data Action = AddSingleNode Node
            | AddSingleEdge (PortRef, PortRef)
            | AddNodes      [Node]
            | AddEdges      [(PortRef, PortRef)]
            | DisplayEdges
            | GraphFetched  [Node] [(PortRef, PortRef)]
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display (GraphFetched nodes edges) = "gf(" <> display nodes <> "," <> display edges <> ")"

data Reaction = PerformIO (IO ())
              | NoOp

instance PrettyPrinter Reaction where
    display _ = "GraphFetcherReaction"

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.GraphViewFetched nodes edges)) = Just $ GraphFetched nodes edges
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt (AddSingleEdge (src, dst)) state = ActionUI NoOp newState where
      newState    = state & Global.graph %~ (addConnection src dst)

    execSt (AddSingleNode node) state = ActionUI (PerformIO action) newState where
      (newState, action) = AddNode.addNode node state

    execSt DisplayEdges state = ActionUI (PerformIO draw) state where
      draw        = displayConnections nodesMap connections
      nodesMap    = state ^. Global.graph & getNodesMap
      connections = state ^. Global.graph & getConnections

    execSt (AddNodes nodes) state = execSt (AddSingleNode <$> nodes) state

    execSt (AddEdges edges) state = execSt (AddSingleEdge <$> edges) state

    execSt (GraphFetched nodes edges) state = execSt [AddNodes nodes, AddEdges edges, DisplayEdges] state

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) _) = action
    updateUI (WithState NoOp _)               = return ()
