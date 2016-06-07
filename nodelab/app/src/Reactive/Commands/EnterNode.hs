module Reactive.Commands.EnterNode
    ( enterNode
    , exitNode
    ) where

import           Utils.PreludePlus

import qualified Batch.Workspace                  as Workspace
import           Reactive.Commands.Command        (Command, performIO)
import           Reactive.Commands.ProjectManager as ProjectManager
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global

import           Empire.API.Data.Breadcrumb       (BreadcrumbItem (..))
import qualified Empire.API.Data.Breadcrumb       as Breadcrumb
import qualified Empire.API.Data.GraphLocation    as GraphLocation

enterNode :: BreadcrumbItem -> Command State ()
enterNode item = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb . Breadcrumb.items %~ (item:)
    ProjectManager.navigateToGraph newLocation

exitNode :: Command State ()
exitNode = do
    location <- use $ Global.workspace . Workspace.currentLocation
    case location ^. GraphLocation.breadcrumb . Breadcrumb.items of
        (_:t) -> ProjectManager.navigateToGraph $ location & GraphLocation.breadcrumb . Breadcrumb.items .~ t
        [] -> return ()
