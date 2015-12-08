module UI.Handlers.Slider.Discrete where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Event.Mouse as Mouse
import           Event.Keyboard (KeyMods(..))
import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Data.Text.Lazy as Text
import qualified Object.Widget.Slider.Discrete as Model
import qualified Object.Widget.TextBox         as TextBox
import           UI.Widget.Slider.Discrete ()
import           UI.Widget.Number (keyModMult)
import           UI.Generic (takeFocus, startDrag)
import           UI.Instances ()
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import qualified Object.Widget.TextBox           as TextBox
import qualified UI.Handlers.TextBox             as TextBox
import           Reactive.State.UIRegistry (addHandler)
import           Data.Text.Lazy.Read (decimal)


isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

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

dblClickHandler :: DblClickHandler Global.State
dblClickHandler evt _ id = do
    enabled <- isEnabled id
    when enabled $ case evt ^. Mouse.keyMods of
        KeyMods True _ _ _ -> do
            width <- inRegistry $ UICmd.get id $ Model.size . x
            let normValue = (evt ^. Mouse.position ^. x) / width
            widget <- inRegistry $ UICmd.update id $ Model.boundedNormValue .~ normValue
            triggerValueChanged (widget ^. Model.value) id
        KeyMods False _ _ _ -> do
            (tbId:_) <- inRegistry $ UICmd.children id
            takeFocus undefined tbId
            inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & dblClick     .~ dblClickHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler

-- Constructors

textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (ValueChangedHandler $ textValueChangedHandler id)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = decimal val
    case val' of
        Left err        -> inRegistry $ do
            val <- UICmd.get parent Model.displayValue
            UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ val)
        Right (val', _) -> do
            inRegistry $ UICmd.update_ parent $ Model.value .~ val'
            triggerValueChanged val' parent


instance CompositeWidget Model.DiscreteSlider where
    createWidget id model = do
        let tx      = (model ^. Model.size . x) / 2.0
            ty      = (model ^. Model.size . y)
            sx      = tx - (model ^. Model.size . y / 2.0)
            textVal = Text.pack $ show $ model ^. Model.value
            textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

        tbId <- UICmd.register id textBox $ textHandlers id
        UICmd.moveX tbId tx

    updateWidget id old model = do
        (tbId:_) <- UICmd.children id
        UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ model ^. Model.displayValue)



