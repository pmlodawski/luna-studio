module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Batch.Workspace                            (InterpreterState (..), interpreterState)
import qualified Event.Batch                                as Batch
import           Event.Event                                (Event (Batch))

import           Reactive.Commands.Command                  (Command, execCommand, performIO)
import           Reactive.Commands.RenderGraph              (renderGraph)
import           Reactive.Plugins.Core.Action.Backend.Graph (isCurrentLocation)
import           Reactive.State.Global                      (State)
import qualified Reactive.State.Global                      as Global

import qualified Batch.Workspace                            as Workspace
import qualified BatchConnector.Monadic.Commands            as BatchCmd
import qualified Empire.API.Data.Graph                      as Graph
import           Empire.API.Data.Node                       (Node)
import           Empire.API.Data.PortRef                    (InPortRef, OutPortRef)
import qualified Empire.API.Graph.GetProgram                as GetProgram
import qualified Empire.API.Update                          as Update

import qualified JS.TextEditor                              as UI



toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.ProgramFetched response)) = Just $ do
    let location = response ^. Update.request . GetProgram.location
    isGraphLoaded  <- use $ Global.workspace . Workspace.isGraphLoaded
    isGoodLocation <- isCurrentLocation location
    when (isGoodLocation && not isGraphLoaded) $ do
        let nodes       = response ^. Update.result . GetProgram.graph . Graph.nodes
            connections = response ^. Update.result . GetProgram.graph . Graph.connections
            code        = response ^. Update.result . GetProgram.code

        renderGraph nodes connections
        performIO $ UI.setText code
        Global.workspace . Workspace.isGraphLoaded .= True
toAction _                                          = Nothing
