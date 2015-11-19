module UI.Handlers.Slider.Discrete where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Event.Mouse as Mouse
import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.Slider.Discrete as Model
import           UI.Widget.Slider.Discrete ()
import           UI.Widget.Number (keyModMult)
import           UI.Generic (takeFocus, startDrag)

newtype ValueChangedHandler = ValueChangedHandler (Int -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey ValueChangedHandler

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

triggerValueChanged :: Int -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

dblClickHandler :: DblClickHandler Global.State
dblClickHandler evt _ id = do
    enabled <- isEnabled id
    when enabled $ do
        width <- inRegistry $ UICmd.get id $ Model.size . x
        let normValue = (evt ^. Mouse.position ^. x) / width
        widget <- inRegistry $ UICmd.update id $ Model.boundedNormValue .~ normValue
        triggerValueChanged (widget ^. Model.value) id

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'W' _ _ id = do
    enabled <- isEnabled id
    when enabled $ do
        widget <- inRegistry $ UICmd.update id $ Model.boundedNormValue +~ 0.1
        triggerValueChanged (widget ^. Model.value) id

keyUpHandler 'Q' _ _ id = do
    enabled <- isEnabled id
    when enabled $ do
        widget <- inRegistry $  UICmd.update id $ Model.boundedNormValue -~ 0.1
        triggerValueChanged (widget ^. Model.value) id

keyUpHandler _ _ _ _ = return ()

startSliderDrag :: MousePressedHandler Global.State
startSliderDrag evt _ id = do
    enabled <- isEnabled id
    when enabled $ do
        value <- inRegistry $ UICmd.get id Model.boundedValue
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
                divider   = width * (keyModMult $ ds ^. keyMods)
                delta     = round $ deltaNorm * (fromIntegral $ widget ^. Model.range)
            inRegistry $ UICmd.update_ id $ Model.boundedValue .~ (startValue + delta)

dragEndHandler :: DragEndHandler Global.State
dragEndHandler _ _ id = do
    enabled <- isEnabled id
    when enabled $ do
        inRegistry $ UICmd.update_ id $ Model.dragStartValue .~ Nothing
        value <- inRegistry $ UICmd.get id Model.boundedValue
        triggerValueChanged value id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & dblClick     .~ dblClickHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler
