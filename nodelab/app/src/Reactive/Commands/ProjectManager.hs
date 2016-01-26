{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.ProjectManager where

import qualified Data.IntMap.Lazy                as IntMap
import qualified Data.Text.Lazy                  as Text
import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (..))

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

displayProjectList :: Command State ()
displayProjectList = do
    groupId <- projectChooserId

    inRegistry $ emptyProjectChooser groupId

    projects <- use $ Global.workspace . Workspace.projects
    forM_ (IntMap.toList projects) $ \(id, project) -> do
        let button = Button.create (Vector2 300 20) $ (Text.pack $ fromMaybe "" $ project ^. Project.name) <> " @ " <> (Text.pack $ project ^. Project.path)
        inRegistry $ UICmd.register groupId button (handle $ ClickedHandler $ const (loadProject id))
