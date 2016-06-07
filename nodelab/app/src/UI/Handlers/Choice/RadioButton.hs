module UI.Handlers.Choice.RadioButton where

import           Utils.PreludePlus

import           Data.HMap.Lazy                   (TypeKey (..))

import qualified Event.Mouse                      as Mouse
import           Object.Widget                    (ClickHandler, UIHandlers, WidgetId, click)

import qualified Object.Widget.Choice.RadioButton as Model
import           Reactive.Commands.Command        (Command)
import qualified Reactive.Commands.UIRegistry     as UICmd
import           Reactive.State.Global            (inRegistry)
import qualified Reactive.State.Global            as Global

import           UI.Generic                       (startDrag)
import           UI.Widget.Toggle                 ()

newtype SelectedHandler = SelectedHandler (Command Global.State ())
selectedHandler = TypeKey :: TypeKey SelectedHandler

triggerSelected :: WidgetId -> Command Global.State ()
triggerSelected id = do
    maybeHandler <- inRegistry $ UICmd.handler id selectedHandler
    withJust maybeHandler $ \(SelectedHandler handler) -> handler

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerSelected id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click  .~ clickHandler
