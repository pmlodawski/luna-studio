module UI.Handlers.Number.Continuous where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget
import           Object.UITypes
import           Event.Event (JSState)

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Data.Text.Lazy as Text
import qualified Object.Widget.Number.Continuous as Model
import qualified Object.Widget.TextBox         as TextBox
import           UI.Widget.Number (keyModMult)
import           UI.Widget.Number.Continuous ()
import           UI.Generic (takeFocus, startDrag)
import           UI.Instances ()

newtype ValueChangedHandler = ValueChangedHandler (Double -> WidgetId -> Command Global.State ())
valueChangedHandlerKey = TypeKey :: TypeKey valueChangedHandler

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

triggerValueChanged :: Double -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id

bumpValue :: Double -> WidgetId -> Command Global.State ()
bumpValue amount id = do
    widget <- zoom Global.uiRegistry $ UICmd.update id (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) id

setValue :: WidgetId -> Double -> Command UIRegistry.State ()
setValue id val = do
    UICmd.update_ id $ Model.value .~ val
    (tbId:_) <- UICmd.children id
    UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ show $ val)

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'Q' _ _ id = bumpValue (-1.0) id
keyUpHandler 'W' _ _ id = bumpValue ( 1.0) id
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
            width <- inRegistry $ UICmd.get id $ Model.size . x
            let diff    = ds ^. currentPos - ds ^. startPos
                delta   = if (abs $ diff ^. x) > (abs $ diff ^. y) then  diff ^. x /  divider
                                                                   else -diff ^. y / (divider * 10.0)
                divider = keyModMult $ ds ^. keyMods
            inRegistry $ setValue id $ startValue + delta

dragEndHandler :: DragEndHandler Global.State
dragEndHandler _ _ id = do
    enabled <- isEnabled id
    when enabled $ do
        inRegistry $ UICmd.update_ id $ Model.dragStartValue .~ Nothing
        value <- inRegistry $ UICmd.get id Model.value
        triggerValueChanged value id

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ id = do
    (tbId:_) <- inRegistry $ UICmd.children id
    takeFocus undefined tbId
    inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & dblClick     .~ dblClickHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler
