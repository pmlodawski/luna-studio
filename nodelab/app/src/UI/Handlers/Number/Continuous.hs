module UI.Handlers.Number.Continuous where

import           Utils.PreludePlus

import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.Number.Continuous as Model
import           UI.Widget.Number (keyModMult)
import           UI.Widget.Number.Continuous ()
import           UI.Generic (takeFocus)

newtype ValueChangedHandler = ValueChangedHandler (Double -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey valueChangedHandler

triggerValueChanged :: Double -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

bumpValue :: Double -> WidgetId -> Command Global.State ()
bumpValue amount id = do
    widget <- zoom Global.uiRegistry $ UICmd.update id (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) id

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'Q' _ id = bumpValue (-1.0) id
keyUpHandler 'W' _ id = bumpValue ( 1.0) id
keyUpHandler _   _ _  = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & mousePressed .~ takeFocus
