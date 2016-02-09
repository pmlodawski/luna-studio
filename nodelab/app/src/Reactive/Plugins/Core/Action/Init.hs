module Reactive.Plugins.Core.Action.Init where

import qualified Data.IntMap.Lazy                 as IntMap
import           Utils.PreludePlus
import           Utils.Vector                     (Vector2 (..))

import           Object.UITypes                   (WidgetId)
import qualified Object.Widget.Group              as Group
import           Reactive.Commands.Command        (Command, execCommand, performIO)
import           Reactive.Commands.ProjectManager (initProjectChooser)
import qualified Reactive.Commands.UIRegistry     as UICmd
import           Reactive.State.Global            (State, inRegistry)
import qualified Reactive.State.Global            as Global
import qualified Reactive.State.UIElements        as UIElements
import           Reactive.State.UIRegistry        (addHandler, handle, sceneInterfaceId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry        as UIRegistry
import           Style.Types                      (uniformPadding)
import qualified UI.Layout                        as Layout

import qualified Style.Layout                     as Style

initSidebar :: Command State WidgetId
initSidebar = do
    let group = Group.create & Group.style .~ Style.sidebar
                             & Group.size .~ Vector2 220 1000
    sidebar <- inRegistry $ UICmd.register sceneInterfaceId group def
    Global.uiElements . UIElements.sidebar .= sidebar
    return sidebar

initBreadcrumb :: Command State WidgetId
initBreadcrumb = do
    let group = Group.create & Group.position .~ Vector2 230 0
                             & Group.style . Group.background ?~ (0.64, 0.21, 0.26)
                             & Group.style . Group.padding    .~ uniformPadding 5.0
    groupId <- inRegistry $ UICmd.register sceneInterfaceId group (Layout.horizontalLayoutHandler 5.0)
    Global.uiElements . UIElements.breadcrumbs .= groupId
    return groupId

initialize :: Command State ()
initialize = do
    sidebarId <- initSidebar
    initBreadcrumb
    void $ initProjectChooser sidebarId
