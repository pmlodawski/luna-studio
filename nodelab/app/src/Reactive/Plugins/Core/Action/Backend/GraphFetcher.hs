module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node
import           Event.Event
import qualified Event.Batch     as Batch
import           Batch.Workspace (interpreterState, InterpreterState(..))

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.State.Global         (State)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import qualified Reactive.Plugins.Core.Action.Commands.AddNode     as AddNode
import           Reactive.Plugins.Core.Action.Commands.Command     (Command, execCommand, performIO)
import           Reactive.Plugins.Core.Action.Commands.RenderGraph (renderGraph)

import qualified BatchConnector.Commands as BatchCmd

toAction :: Event Node -> State -> Maybe (Command State ())
toAction (Batch (Batch.GraphViewFetched nodes edges)) state = Just $ showGraph nodes edges
toAction _ _ = Nothing

showGraph :: [Node] -> [(PortRef, PortRef)] -> Command State ()
showGraph nodes edges = do
    renderGraph nodes edges
    prepareValues nodes

prepareValues :: [Node] -> Command State ()
prepareValues nodes = do
    let nonModules = filter (not . isModule) nodes
    workspace <- use Global.workspace
    case workspace ^. interpreterState of
        Fresh  -> performIO $ BatchCmd.insertSerializationModes workspace nonModules
        AllSet -> performIO $ do
            BatchCmd.runMain
            BatchCmd.requestValues workspace nonModules
