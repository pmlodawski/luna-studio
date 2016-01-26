{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.ProjectManager where

import qualified Data.IntMap.Lazy                as IntMap
import qualified Data.Text.Lazy                  as Text
import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (..), x, y)

import           Object.UITypes (WidgetId)
import qualified Batch.Workspace                 as Workspace
import qualified BatchConnector.Commands         as BatchCmd
import           Reactive.Commands.Command       (Command, execCommand, performIO)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.Commands.UnrenderGraph (unrender)
import           Reactive.State.Global           (State, inRegistry)
import qualified Reactive.State.Global           as Global
import           Reactive.State.UIRegistry       (addHandler, handle, sceneInterfaceId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry       as UIRegistry
import qualified Reactive.State.UIElements       as UIElements

import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Data.Project         as Project

import qualified Object.Widget.Button            as Button
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import qualified Object.Widget.Group             as Group
import           UI.Handlers.Button              (ClickedHandler (..))
import           UI.Instances
import qualified UI.Layout                       as Layout

loadProject :: ProjectId -> Command State ()
loadProject projId = do
    currentProjectId <- use $ Global.workspace . Workspace.currentLocation . GraphLocation.projectId
    when (currentProjectId /= projId) $ do
        unrender
        Global.workspace . Workspace.currentLocation . GraphLocation.projectId .= projId
        workspace <- use Global.workspace
        performIO $ BatchCmd.getProgram workspace

        displayProjectList

projectChooserId :: Command State WidgetId
projectChooserId = do
    projectChooser <- use $ Global.uiElements . UIElements.projectChooser
    case projectChooser of
        0 -> do
            let group = Group.createWithBg (0.3, 0.0, 0.5)
            projectChooser <- inRegistry $ UICmd.register sceneInterfaceId group (Layout.verticalLayoutHandler (Vector2 5.0 5.0) 5.0)
            Global.uiElements . UIElements.projectChooser .= projectChooser
            return projectChooser
        _ -> return projectChooser


emptyProjectChooser :: WidgetId -> Command UIRegistry.State ()
emptyProjectChooser pc = do
    children <- UICmd.children pc
    mapM_ UICmd.removeWidget children

openAddProjectDialog :: Command State ()
openAddProjectDialog = inRegistry $ do
    let group = (Group.createWithBg (0.3, 0.3, 0.5)) & Group.position . x .~ 230.0

    groupId <- UICmd.register sceneInterfaceId group (Layout.horizontalLayoutHandler (Vector2 5.0 5.0) 5.0)

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

    let button = Button.create (Vector2 200 20) "Create project"
    inRegistry $ UICmd.register groupId button (handle $ ClickedHandler $ const openAddProjectDialog)


    projects <- use $ Global.workspace . Workspace.projects
    forM_ (IntMap.toList projects) $ \(id, project) -> do
        let isCurrent = if (currentProjectId == id) then "* " else ""
        let button = Button.create (Vector2 200 20) $ (isCurrent <> (Text.pack $ fromMaybe "(no name)" $ project ^. Project.name))
        inRegistry $ UICmd.register groupId button (handle $ ClickedHandler $ const (loadProject id))
