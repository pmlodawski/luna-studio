{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Breadcrumbs (
    update
) where

import qualified Data.Text.Lazy                as Text
import           Utils.PreludePlus
import           Utils.Vector                  (Vector2 (..), x, y)

import qualified Batch.Workspace               as Workspace
import qualified JS.UI                         as JS
import qualified Object.Widget.Button          as Button
import           Object.Widget.Icon            (Icon (..))
import           Reactive.Commands.Command     (Command)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (State, inRegistry)
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.UIElements     as UIElements
import           Reactive.State.UIRegistry     (addHandler)
import qualified Style.Layout                  as Style
import           UI.Handlers.Button            (ClickedHandler (..))
import           UI.Instances                  ()

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..))
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import qualified Empire.API.Data.GraphLocation as GraphLocation
import qualified Empire.API.Data.Project       as Project


destroyBreadcrumbs :: Command State ()
destroyBreadcrumbs = do
    breadcrumbs <- use $ Global.uiElements . UIElements.breadcrumbs
    breadcrumbItems <- inRegistry $ UICmd.children breadcrumbs
    inRegistry  $ mapM_ UICmd.removeWidget breadcrumbItems

displayBreadcrumbs :: (Breadcrumb -> Command State ()) -> Command State ()
displayBreadcrumbs enterBreadcrumbs = do
    group <- use $ Global.uiElements . UIElements.breadcrumbs

    currentBreadcrumb <- use $ Global.workspace . Workspace.currentLocation . GraphLocation.breadcrumb . Breadcrumb.items

    currentProjectName <- use $ Global.workspace . Workspace.currentProject . Project.name

    inRegistry $ do
        forM_ (reverse $ tails currentBreadcrumb) $ \bc -> do
            let name = case bc of
                    (item:_) -> case item of
                        Breadcrumb.Function name -> name
                        Breadcrumb.Module   name -> name
                    [] -> currentProjectName
                name'  = Text.pack name
                widget = Button.create Style.breadcrumbItemSize name'
                       & Button.style .~ Style.breadcrumbItemStyle
                       & Button.size  . x .~ (fromIntegral $ 5 + JS.calculateTextWidth name')
                handlers = addHandler (ClickedHandler $ \_ -> enterBreadcrumbs $ Breadcrumb bc) mempty
            when (length bc /= 0) $ UICmd.register_ group (Icon def (Vector2 20 20) "triangle") def
            UICmd.register group widget handlers


update :: (Breadcrumb -> Command State ()) -> Command State ()
update enterBreadcrumbs = do
    destroyBreadcrumbs
    displayBreadcrumbs enterBreadcrumbs


