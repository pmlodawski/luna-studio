module Reactive.Plugins.Core.Action.Backend.ProjectManager where

import           Utils.PreludePlus
import qualified Data.IntMap.Lazy                 as IntMap


import qualified Event.Batch                      as Batch
import           Event.Event                      (Event (Init, Batch))

import           Reactive.Commands.Command        (Command, execCommand, performIO)
import           Reactive.Commands.ProjectManager (loadGraph, loadProject)
import           Reactive.Commands.RenderGraph    (renderGraph)
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global

import qualified Object.Widget.Button             as Button

import qualified Batch.Workspace                  as Workspace
import qualified Empire.API.Data.Graph            as Graph
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Node             (Node)
import           Empire.API.Data.PortRef          (InPortRef, OutPortRef)
import qualified Empire.API.Project.ListProjects  as ListProjects
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Response              as Response

whenOk :: Response.Response req res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ req (Response.Ok res))  handler = handler res
whenOk (Response.Response _ req (Response.Error _)) _       = return ()

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.ProjectList response)) = Just $ do
    whenOk response $ \(ListProjects.Result projects) -> do
        Global.workspace . Workspace.projects .= IntMap.fromList projects

        lastLocation <- use $ Global.workspace . Workspace.lastUILocation
        withJust lastLocation $ \lastLocation -> do
            let lastGraphLocation = Workspace.fromUIGraphLocation projects lastLocation
            withJust lastGraphLocation $ assign (Global.workspace . Workspace.currentLocation)
        loc <- use $ Global.workspace . Workspace.currentLocation
        loadGraph loc

toAction (Batch (Batch.ProjectCreated response)) = Just $ do
    whenOk response $ \(CreateProject.Result projectId project) -> do
        Global.workspace . Workspace.projects . at projectId ?= project
        projs <- use $ Global.workspace . Workspace.projects
        loadProject projectId
toAction _ = Nothing

