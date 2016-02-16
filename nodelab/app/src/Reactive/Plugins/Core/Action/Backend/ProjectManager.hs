module Reactive.Plugins.Core.Action.Backend.ProjectManager where

import           Utils.PreludePlus
import qualified Data.IntMap.Lazy                 as IntMap

import qualified Event.Batch                      as Batch
import           Event.Event                      (Event (Init, Batch))

import           Reactive.Commands.Command        (Command, execCommand, performIO)
import           Reactive.Commands.ProjectManager (updateProjectList, loadGraph, loadProject)
import           Reactive.Commands.RenderGraph    (renderGraph)
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global

import qualified Object.Widget.Button             as Button

import qualified Batch.Workspace                  as Workspace
import qualified BatchConnector.Commands          as BatchCmd
import qualified Empire.API.Data.Graph            as Graph
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Node             (Node)
import           Empire.API.Data.PortRef          (InPortRef, OutPortRef)
import qualified Empire.API.Project.ListProjects  as ListProjects
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Update                as Update



toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.ProjectList response)) = Just $ do
    let projects = response ^. Update.result . ListProjects.projects
    Global.workspace . Workspace.projects .= IntMap.fromList projects


    lastLocation <- use $ Global.workspace . Workspace.lastUILocation
    withJust lastLocation $ \lastLocation -> do
        let lastGraphLocation = Workspace.fromUIGraphLocation projects lastLocation
        withJust lastGraphLocation $ assign (Global.workspace . Workspace.currentLocation)

    loc <- use $ Global.workspace . Workspace.currentLocation

    updateProjectList
    loadGraph loc

toAction (Batch (Batch.ProjectCreated response)) = Just $ do
    let pid     = response ^. Update.result . CreateProject.projectId
        project = response ^. Update.result . CreateProject.project
    Global.workspace . Workspace.projects . at pid ?= project

    projs <- use $ Global.workspace . Workspace.projects

    updateProjectList

    loadProject pid
toAction _ = Nothing

