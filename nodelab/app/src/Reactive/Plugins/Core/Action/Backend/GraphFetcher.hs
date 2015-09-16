module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node
import           Event.Event
import qualified Event.Batch as Batch

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.Executors.Graph   (displayConnections)
import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Global      as Global
import qualified Reactive.Plugins.Core.Action.Executors.AddNode as AddNode

import qualified BatchConnector.Commands as BatchCmd

data Action = AddSingleNode Node
            | AddSingleEdge (PortRef, PortRef)
            | AddNodes      [Node]
            | AddEdges      [(PortRef, PortRef)]
            | DisplayEdges
            | RequestValues [Node]
            | GraphFetched  [Node] [(PortRef, PortRef)]
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display (GraphFetched nodes edges) = "gf(" <> display nodes <> "," <> display edges <> ")"

data Reaction = PerformIO (IO ())
              | NoOp

instance PrettyPrinter Reaction where
    display _ = "GraphFetcherReaction"

toAction :: Event Node -> Global.State -> Maybe Action
toAction (Batch (Batch.GraphViewFetched nodes edges)) state = case getNodes $ state ^. Global.graph of
    [] -> Just $ GraphFetched nodes edges
    _  -> Nothing
toAction _ _ = Nothing

instance ActionStateUpdater Action where
    execSt (AddSingleEdge (src, dst)) state = ActionUI NoOp newState where
        newState    = state & Global.graph %~ (addConnection src dst)

    execSt (AddSingleNode node) state = ActionUI (PerformIO addAction) newState where
        (newState, addAction) = AddNode.addNode node state

    execSt DisplayEdges state = ActionUI (PerformIO draw) state where
        draw        =  displayConnections nodesMap connectionsMap
                    >> BatchCmd.runMain
        nodesMap       = state ^. Global.graph & getNodesMap
        connectionsMap = state ^. Global.graph & getConnectionsMap

    execSt (RequestValues nodes) state = ActionUI (PerformIO $ BatchCmd.requestValues workspace nodes) state where
        workspace = state ^. Global.workspace

    execSt (AddNodes nodes) state = execSt (AddSingleNode <$> nodes) state

    execSt (AddEdges edges) state = execSt (AddSingleEdge <$> edges) state

    execSt (GraphFetched nodes edges) state = execSt [AddNodes nodes, AddEdges edges, DisplayEdges, RequestValues nodes] state

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) _) = action
    updateUI (WithState NoOp _)               = return ()
