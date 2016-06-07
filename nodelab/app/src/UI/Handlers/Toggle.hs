module UI.Handlers.Toggle where

import           Utils.PreludePlus

import           Object.Widget                (ClickHandler, KeyUpHandler, UIHandlers, WidgetId, click, keyUp)
import qualified Object.Widget.Toggle         as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.Toggle             ()

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

toggleValue :: WidgetId -> Command Global.State ()
toggleValue id = do
    enabled <- isEnabled id
    when enabled $ do
        widget <- inRegistry $ UICmd.update id $ Model.value %~ not
        triggerValueChanged (widget ^. Model.value) id

clickHandler :: ClickHandler Global.State
clickHandler _ _ = toggleValue

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'r' _ _ id = toggleValue id
keyUpHandler _ _ _ _ = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp  .~ keyUpHandler
                     & click  .~ clickHandler
