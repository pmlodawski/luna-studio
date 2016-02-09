{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.ProjectManager where

import qualified Data.IntMap.Lazy                as IntMap
import qualified Data.Text.Lazy                  as Text
import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (..), x, y)

import qualified Batch.Workspace                 as Workspace
import qualified BatchConnector.Commands         as BatchCmd
import           Object.UITypes                  (WidgetId)
import qualified Reactive.Commands.Breadcrumbs   as Breadcrumbs
import           Reactive.Commands.Command       (Command, execCommand, performIO)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.Commands.UnrenderGraph (unrender)
import           Reactive.State.Global           (State, inRegistry)
import qualified Reactive.State.Global           as Global
import qualified Reactive.State.UIElements       as UIElements
import           Reactive.State.UIRegistry       (addHandler, handle, sceneInterfaceId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry       as UIRegistry

import           Empire.API.Data.Breadcrumb      (Breadcrumb (..))
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Data.Project         as Project

import qualified Object.Widget.Button            as Button
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import           Style.Types                     (uniformPadding)
import qualified Style.Layout as Style
import           UI.Handlers.Button              (ClickedHandler (..))
import           UI.Instances
import qualified UI.Layout                       as Layout

loadProject :: ProjectId -> Command State ()
loadProject projId = do
    let newLocation = GraphLocation projId 0 (Breadcrumb [])
    loadGraph newLocation
    displayProjectList

loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    currentLocation <- use $ Global.workspace . Workspace.currentLocation
    when (currentLocation /= location) $ do
        unrender
        Global.workspace . Workspace.currentLocation .= location
        saveCurrentLocation
        Breadcrumbs.update enterBreadcrumbs
        workspace <- use Global.workspace
        performIO $ BatchCmd.getProgram workspace

enterBreadcrumbs :: Breadcrumb -> Command State ()
enterBreadcrumbs newBc = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb .~ newBc
    loadGraph newLocation


saveCurrentLocation :: Command State ()
saveCurrentLocation = return () -- TODO: Save current location to browser localStorage

projectChooserId :: Command State WidgetId
projectChooserId = use $ Global.uiElements . UIElements.projectChooser

initProjectChooser :: WidgetId -> Command State WidgetId
initProjectChooser container = do
    let button = Button.create (Vector2 210 20) "Create project"
    inRegistry $ UICmd.register container button (handle $ ClickedHandler $ const openAddProjectDialog)

    let group = Group.create & Group.position . y .~ 40.0
                             & Group.style .~ Style.projectChooser

    projectChooser <- inRegistry $ UICmd.register container group (Layout.verticalLayoutHandler 5.0)
    Global.uiElements . UIElements.projectChooser .= projectChooser

    return projectChooser

emptyProjectChooser :: WidgetId -> Command UIRegistry.State ()
emptyProjectChooser pc = do
    children <- UICmd.children pc
    mapM_ UICmd.removeWidget children

openAddProjectDialog :: Command State ()
openAddProjectDialog = inRegistry $ do
    let group = Group.create & Group.position . x .~ 230.0
                             & Group.style . Group.background ?~ (0.3, 0.3, 0.5)
                             & Group.style . Group.padding .~ uniformPadding 5.0
    groupId <- UICmd.register sceneInterfaceId group (Layout.horizontalLayoutHandler 5.0)

    let tb = LabeledTextBox.create (Vector2 200 20) "Project name" "Untitled project"
    tbId <- UICmd.register groupId tb def

    let button = Button.create (Vector2 100 20) "Create project"
    UICmd.register_ groupId button $ handle $ ClickedHandler $ const $ do
        name <- inRegistry $ UICmd.get tbId LabeledTextBox.value
        inRegistry $ UICmd.removeWidget groupId
        performIO $ BatchCmd.createProject name $ name <> ".luna"

    let button = Button.create (Vector2 80 20) "Cancel"
    UICmd.register_ groupId button $ handle $ ClickedHandler $ const $ do
        inRegistry $ UICmd.removeWidget groupId



displayProjectList :: Command State ()
displayProjectList = do
    groupId <- projectChooserId

    inRegistry $ emptyProjectChooser groupId

    currentProjectId <- use $ Global.workspace . Workspace.currentLocation . GraphLocation.projectId

    projects <- use $ Global.workspace . Workspace.projects
    forM_ (IntMap.toList projects) $ \(id, project) -> do
        let isCurrent = if (currentProjectId == id) then "* " else ""
        let button = Button.create (Vector2 200 20) $ (isCurrent <> (Text.pack $ fromMaybe "(no name)" $ project ^. Project.name))
        inRegistry $ UICmd.register groupId button (handle $ ClickedHandler $ const (loadProject id))
