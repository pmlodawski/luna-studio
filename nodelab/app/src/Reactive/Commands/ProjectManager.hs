{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.ProjectManager where

import qualified Data.IntMap.Lazy                as IntMap
import qualified Data.Text.Lazy                  as Text
import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (..), x, y)

import qualified Batch.Workspace                 as Workspace
import qualified Reactive.Commands.Batch         as BatchCmd
import           Object.UITypes                  (WidgetId)
import qualified Reactive.Commands.Breadcrumbs   as Breadcrumbs
import           Reactive.Commands.Command       (Command, execCommand, performIO)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.Commands.UnrenderGraph (unrender)
import           Reactive.State.Global           (State, inRegistry)
import qualified Reactive.State.Global           as Global
import qualified Reactive.State.UIElements       as UIElements
import           Reactive.State.UIRegistry       (addHandler, handle, sceneInterfaceId)
import qualified Reactive.State.UIRegistry       as UIRegistry

import           Empire.API.Data.Breadcrumb      (Breadcrumb (..))
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Data.Project         as Project

import qualified JS.GraphLocation                as JS
import qualified Object.Widget.Button            as Button
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import qualified Style.Layout                    as Style
import           Style.Types                     (uniformPadding)
import           UI.Handlers.Button              (ClickedHandler (..))
import           UI.Instances
import qualified UI.Layout                       as Layout


loadProject :: ProjectId -> Command State ()
loadProject projId = do
    let newLocation = GraphLocation projId 0 (Breadcrumb [])
    navigateToGraph newLocation

loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    currentLocation <- use $ Global.workspace . Workspace.currentLocation
    unrender
    Global.workspace . Workspace.currentLocation .= location
    saveCurrentLocation
    displayCurrentBreadcrumb
    BatchCmd.getProgram

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    currentLocation <- use $ Global.workspace . Workspace.currentLocation
    when (currentLocation /= location) $ loadGraph location

displayCurrentBreadcrumb :: Command State ()
displayCurrentBreadcrumb = Breadcrumbs.update enterBreadcrumbs

enterBreadcrumbs :: Breadcrumb -> Command State ()
enterBreadcrumbs newBc = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb .~ newBc
    navigateToGraph newLocation

saveCurrentLocation :: Command State ()
saveCurrentLocation = do
    workspace <- use $ Global.workspace
    performIO $ JS.saveLocation $ workspace ^. Workspace.uiGraphLocation
