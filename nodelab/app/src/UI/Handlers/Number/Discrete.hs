module UI.Handlers.Number.Discrete where

import           Utils.PreludePlus

import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.Number.Discrete as Model
import           UI.Widget.Number (keyModMult)
import           UI.Generic (takeFocus)
import           UI.Widget.Number.Discrete ()

newtype ValueChangedHandler = ValueChangedHandler (Int -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey valueChangedHandler

triggerValueChanged :: Int -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

bumpValue :: Int -> WidgetId -> Command Global.State ()
bumpValue amount id = do
    widget <- zoom Global.uiRegistry $ UICmd.update id (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) id

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'Q' _ id = bumpValue (-1) id
keyUpHandler 'W' _ id = bumpValue ( 1) id
keyUpHandler _   _ _  = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & mousePressed .~ takeFocus
