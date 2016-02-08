module Reactive.Plugins.Core.Action.Init where

import qualified Data.IntMap.Lazy                 as IntMap
import           Utils.PreludePlus
import           Utils.Vector                     (Vector2 (..))

import           Object.UITypes                   (WidgetId)
import qualified Object.Widget.Group              as Group
import           Reactive.Commands.Command        (Command, execCommand, performIO)
import           Reactive.Commands.ProjectManager (initProjectChooser)
import qualified Reactive.Commands.UIRegistry     as UICmd
import qualified UI.Layout                        as Layout
import           Reactive.State.Global            (State, inRegistry)
import qualified Reactive.State.Global            as Global
import qualified Reactive.State.UIElements        as UIElements
import           Reactive.State.UIRegistry        (addHandler, handle, sceneInterfaceId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry        as UIRegistry


initSidebar :: Command State WidgetId
initSidebar = do
    let group = Group.createWithBg (0.64, 0.21, 0.26) & Group.size .~ Vector2 220 1000
    sidebar <- inRegistry $ UICmd.register sceneInterfaceId group def
    Global.uiElements . UIElements.sidebar .= sidebar
    return sidebar

initBreadcrumb :: Command State WidgetId
initBreadcrumb = do
    let group = Group.createWithBg (0.64, 0.21, 0.26) & Group.position .~ Vector2 230 0
    groupId <- inRegistry $ UICmd.register sceneInterfaceId group (Layout.horizontalLayoutHandler (Vector2 3.0 3.0) 3.0)
    Global.uiElements . UIElements.breadcrumbs .= groupId
    return groupId

initialize :: Command State ()
initialize = do
    sidebarId <- initSidebar
    initBreadcrumb
    void $ initProjectChooser sidebarId
