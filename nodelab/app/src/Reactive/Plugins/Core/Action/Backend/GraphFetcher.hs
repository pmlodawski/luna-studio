module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node
import           Event.Event
import           Batch.Workspace
import qualified Event.Batch     as Batch
import qualified Data.Map        as Map
import           Utils.Vector    (Vector2(..))
import           Utils.Graph.AutoLayout

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import qualified Reactive.Plugins.Core.Action.Commands.AddNode     as AddNode
import           Reactive.Plugins.Core.Action.Commands.Command     (execCommand)
import           Reactive.Plugins.Core.Action.Commands.RenderGraph (renderGraph)

import qualified BatchConnector.Commands as BatchCmd

data Action = ShowGraph [Node] [(PortRef, PortRef)]
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
    execSt (ShowGraph nodes edges) state = ActionUI (PerformIO action) newState where
        (action, newState) = execCommand (renderGraph nodes edges) state

    execSt (GraphFetched nodes edges) state = execSt [ShowGraph nodes edges, PrepareValues nodes] state

    execSt RequestRun state = ActionUI (PerformIO BatchCmd.runMain) state

    execSt (PrepareValues nodes) state = ActionUI (PerformIO prepareValues) state where
        workspace     = state ^. Global.workspace
        prepareValues = case workspace ^. interpreterState of
            Fresh  -> BatchCmd.insertSerializationModes workspace nodes
            AllSet -> BatchCmd.runMain
                   >> BatchCmd.requestValues workspace nodes


instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO action) _) = action
    updateUI (WithState NoOp _)               = return ()
