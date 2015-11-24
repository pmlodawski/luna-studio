module UI.Handlers.Toggle where

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
import qualified Object.Widget.Toggle as Model
import           UI.Widget.Toggle ()
import           UI.Generic (takeFocus, startDrag)

newtype ValueChangedHandler = ValueChangedHandler (Bool -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey ValueChangedHandler

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

triggerValueChanged :: Bool -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

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
