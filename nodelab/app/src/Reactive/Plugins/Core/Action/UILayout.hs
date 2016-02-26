module Reactive.Plugins.Core.Action.UILayout where

import           Utils.PreludePlus
import           Utils.Vector                 (Vector2 (..), x, y)

import           Event.Event                  (Event (Window))
import qualified Event.Window                 as Window
import           Object.UITypes               (WidgetId)
import qualified Object.Widget.Group          as Group
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Camera        as Camera
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIElements    as UIElements

import qualified Style.Layout                 as Style
import qualified UI.Layout                    as Layout

toAction :: Event -> Maybe (Command Global.State ())
toAction (Window (Window.Event Window.Resized width height)) = Just $ resizeSidebar
toAction _ = Nothing

resizeSidebar :: Command State ()
resizeSidebar = do
    screenSize <- use $ Global.camera . Camera.camera . Camera.screenSize
    sidebarId <- use $ Global.uiElements . UIElements.sidebar
    inRegistry $ UICmd.resize sidebarId (Vector2 Style.sidebarWidth $ fromIntegral $ screenSize ^. y)

    bcId <- use $ Global.uiElements . UIElements.breadcrumbs
    inRegistry $ UICmd.resize bcId (Vector2 (fromIntegral $ screenSize ^. x) Style.breadcrumbsHeight)
