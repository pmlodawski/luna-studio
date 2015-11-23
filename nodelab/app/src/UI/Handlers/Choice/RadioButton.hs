module UI.Handlers.Choice.RadioButton where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Event.Mouse as Mouse
import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.Choice.RadioButton as Model
import           UI.Widget.Toggle ()
import           UI.Generic (takeFocus, startDrag)

newtype SelectedHandler = SelectedHandler (Command Global.State ())
selectedHandler = TypeKey :: TypeKey selectedHandler

triggerSelected :: WidgetId -> Command Global.State ()
triggerSelected id = do
    maybeHandler <- inRegistry $ UICmd.handler id selectedHandler
    forM_ maybeHandler $ \(SelectedHandler handler) -> handler

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerSelected id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click  .~ clickHandler
