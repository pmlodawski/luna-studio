module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Object.Node     (Node, isModule, PortRef)
import           Event.Event     (Event(Batch))
import qualified Event.Batch     as Batch
import           Batch.Workspace (interpreterState, InterpreterState(..))

import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State)
import           Reactive.Commands.Command     (Command, execCommand, performIO)
import           Reactive.Commands.RenderGraph (renderGraph)

import qualified BatchConnector.Monadic.Commands as BatchCmd

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.GraphViewFetched nodes edges)) = Just $ showGraph nodes edges
toAction _                                            = Nothing

showGraph :: [Node] -> [(PortRef, PortRef)] -> Command State ()
showGraph nodes edges = do
    renderGraph nodes edges
    prepareValues nodes

prepareValues :: [Node] -> Command State ()
prepareValues nodes = do
    let nonModules = filter (not . isModule) nodes
    workspace <- use Global.workspace
    case workspace ^. interpreterState of
        Fresh  -> zoom Global.workspace $ BatchCmd.insertSerializationModes nonModules
        AllSet -> zoom Global.workspace $ do
            BatchCmd.runMain
            BatchCmd.requestValues nonModules
