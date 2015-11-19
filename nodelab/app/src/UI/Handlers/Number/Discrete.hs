module UI.Handlers.Number.Discrete where

import           Utils.PreludePlus
import           Utils.Vector

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
import           UI.Generic (takeFocus, startDrag)
import           UI.Widget.Number.Discrete ()

newtype ValueChangedHandler = ValueChangedHandler (Int -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey valueChangedHandler

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled


triggerValueChanged :: Int -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

bumpValue :: Int -> WidgetId -> Command Global.State ()
bumpValue amount id = do
    widget <- zoom Global.uiRegistry $ UICmd.update id (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) id

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'Q' _ _ id = bumpValue (-1) id
keyUpHandler 'W' _ _ id = bumpValue ( 1) id
keyUpHandler _   _ _ _  = return ()

startSliderDrag :: MousePressedHandler Global.State
startSliderDrag evt _ id = do
    enabled <- isEnabled id
    when enabled $ do
        value <- inRegistry $ UICmd.get id Model.value
        inRegistry $ UICmd.update_ id $ Model.dragStartValue .~ (Just value)
        startDrag evt id

dragHandler :: DragMoveHandler Global.State
dragHandler ds _ id = do
    enabled <- isEnabled id
    when enabled $ do
        startValue <- inRegistry $ UICmd.get id Model.dragStartValue
        forM_ startValue $ \startValue -> do
            widget <- inRegistry $ UICmd.lookup id
            let width     = widget ^. Model.size . x
                diff      = ds ^. currentPos - ds ^. startPos
                deltaNorm = if (abs $ diff ^. x) > (abs $ diff ^. y) then  diff ^. x /  divider
                                                                     else -diff ^. y / (divider * 10.0)
                divider   = keyModMult $ ds ^. keyMods
                delta     = round $ deltaNorm
            inRegistry $ UICmd.update_ id $ Model.value .~ (startValue + delta)

dragEndHandler :: DragEndHandler Global.State
dragEndHandler _ _ id = do
    enabled <- isEnabled id
    when enabled $ do
        inRegistry $ UICmd.update_ id $ Model.dragStartValue .~ Nothing
        value <- inRegistry $ UICmd.get id Model.value
        triggerValueChanged value id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler
