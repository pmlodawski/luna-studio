module Reactive.Plugins.Core.Action.Backend.GraphFetcher where

import           Utils.PreludePlus

import           Event.Event     (Event(Batch))
import qualified Event.Batch     as Batch
import           Batch.Workspace (interpreterState, InterpreterState(..))

import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State)
import           Reactive.Commands.Command     (Command, execCommand, performIO)
import           Reactive.Commands.RenderGraph (renderGraph)

import qualified BatchConnector.Monadic.Commands as BatchCmd
import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Graph as Graph
import           Empire.API.Data.PortRef (OutPortRef, InPortRef)
import qualified Empire.API.Graph.GetProgram as GetProgram
import qualified Empire.API.Response       as Response
import qualified Batch.Workspace           as Workspace
import           Reactive.Plugins.Core.Action.Backend.AddNode (isCurrentLocation)
import qualified JS.TextEditor    as UI



toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.ProgramFetched response)) = Just $ do
    let location = response ^. Response.request . GetProgram.location
    isGraphLoaded  <- use $ Global.workspace . Workspace.isGraphLoaded
    isGoodLocation <- isCurrentLocation location
    when (isGoodLocation && not isGraphLoaded) $ do
        let nodes       = response ^. Response.update . GetProgram.graph . Graph.nodes
            connections = response ^. Response.update . GetProgram.graph . Graph.connections
            code        = response ^. Response.update . GetProgram.code

        renderGraph nodes connections
        performIO $ UI.setText code
        Global.workspace . Workspace.isGraphLoaded .= True
toAction _                                          = Nothing

-- showGraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
-- showGraph nodes edges = do
--     renderGraph nodes edges
    -- prepareValues nodes

-- isModule _ = False -- TODO

-- prepareValues :: [Node] -> Command State ()
-- prepareValues nodes = do
--     let nonModules = filter (not . isModule) nodes
--     workspace <- use Global.workspace
--     case workspace ^. interpreterState of
--         Fresh  -> zoom Global.workspace $ BatchCmd.insertSerializationModes nonModules
--         AllSet -> zoom Global.workspace $ do
--             BatchCmd.runMain
--             BatchCmd.requestValues nonModules
