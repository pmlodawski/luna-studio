module UI.Handlers.Button where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (TypeKey (..))

import           Object.Widget                (WidgetId, ClickHandler, click, UIHandlers)
import qualified Object.Widget.Button         as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           UI.Generic                   (takeFocus)
import           UI.Widget.Toggle             ()

newtype ClickedHandler = ClickedHandler (WidgetId -> Command Global.State ())
clickedHandler = TypeKey :: TypeKey ClickedHandler

triggerClicked :: WidgetId -> Command Global.State ()
triggerClicked id = do
    maybeHandler <- inRegistry $ UICmd.handler id clickedHandler
    forM_ maybeHandler $ \(ClickedHandler handler) -> handler id

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerClicked id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click .~ clickHandler
