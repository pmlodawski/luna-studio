{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Breadcrumbs (
    update
) where

import qualified Data.IntMap.Lazy              as IntMap
import qualified Data.Text.Lazy                as Text
import           Utils.PreludePlus
import           Utils.Vector                  (Vector2 (..), x, y)

import qualified Batch.Workspace               as Workspace
import qualified BatchConnector.Commands       as BatchCmd
import           Object.UITypes                (WidgetId)
import           Reactive.Commands.Command     (Command, execCommand, performIO)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (State, inRegistry)
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.UIElements     as UIElements
import           Reactive.State.UIRegistry     (addHandler, handle, sceneInterfaceId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry     as UIRegistry

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..))
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import qualified Empire.API.Data.GraphLocation as GraphLocation

import qualified Object.Widget.Button          as Button
import qualified Object.Widget.Group           as Group
import qualified Object.Widget.LabeledTextBox  as LabeledTextBox
import qualified Style.Layout                  as Style
import           UI.Handlers.Button            (ClickedHandler (..))
import           UI.Instances
import qualified UI.Layout                     as Layout

destroyBreadcrumbs :: Command State ()
destroyBreadcrumbs = do
    breadcrumbs <- use $ Global.uiElements . UIElements.breadcrumbs
    breadcrumbItems <- inRegistry $ UICmd.children breadcrumbs
    inRegistry  $ mapM_ UICmd.removeWidget breadcrumbItems

displayBreadcrumbs :: (Breadcrumb -> Command State ()) -> Command State ()
displayBreadcrumbs enterBreadcrumbs = do
    group <- use $ Global.uiElements . UIElements.breadcrumbs

    currentBreadcrumb <- use $ Global.workspace . Workspace.currentLocation . GraphLocation.breadcrumb . Breadcrumb.items

    inRegistry $ do
        forM_ (reverse $ tails currentBreadcrumb) $ \bc -> do
            let name = case bc of
                    (item:_) -> case item of
                        Breadcrumb.Function name -> name
                        Breadcrumb.Module   name -> name
                    [] -> "(Project)"
                widget = Button.create Style.breadcrumbItemSize (Text.pack name)
                       & Button.style .~ Style.breadcrumbItemStyle
                handlers = addHandler (ClickedHandler $ \_ -> enterBreadcrumbs $ Breadcrumb bc) mempty
            UICmd.register group widget handlers


update :: (Breadcrumb -> Command State ()) -> Command State ()
update enterBreadcrumbs = do
    destroyBreadcrumbs
    displayBreadcrumbs enterBreadcrumbs


