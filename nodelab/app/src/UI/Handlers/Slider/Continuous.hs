module UI.Handlers.Slider.Continuous where

import           Utils.PreludePlus

import           Utils.Vector                    (Vector2(..), x, y)
import           Data.HMap.Lazy                  (HTMap, TypeKey (..))
import qualified Data.Text.Lazy                  as Text
import           Data.Text.Lazy.Read             (rational)

import           Event.Keyboard                  (KeyMods (..), shift)
import qualified Event.Mouse                     as Mouse
import           Object.UITypes                  (WidgetId)
import           Object.Widget                   (DblClickHandler, DragEndHandler, DragMoveHandler, KeyUpHandler,
                                                  MousePressedHandler, UIHandlers, currentPos, dblClick, dragEnd,
                                                  dragMove, keyMods, keyUp, mousePressed, startPos, click,
                                                  ClickHandler, CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)
import qualified Object.Widget.Slider.Continuous as Model
import qualified Object.Widget.TextBox           as TextBox
import           Reactive.Commands.Command       (Command, performIO)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.State.Global           (inRegistry)
import qualified Reactive.State.Global           as Global
import           Reactive.State.UIRegistry       (addHandler)
import qualified Reactive.State.UIRegistry       as UIRegistry

import           UI.Generic                      (startDrag, whenChanged)
import           UI.Handlers.Generic             (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.TextBox             as TextBox
import           UI.Widget.Number                (keyModMult)
import           UI.Widget.Slider.Continuous     ()

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
        widget <- inRegistry $ UICmd.update id $ Model.boundedNormValue -~ 0.1
        triggerValueChanged (widget ^. Model.value) id

keyUpHandler _ _ _ _ = return ()

startSliderDrag :: MousePressedHandler Global.State
startSliderDrag evt _ id = do
    enabled <- isEnabled id
    when (enabled && (evt ^. Mouse.button == Mouse.LeftButton)) $ do
        value <- inRegistry $ UICmd.get id Model.boundedNormValue
        inRegistry $ UICmd.update_ id $ Model.dragStartValue .~ (Just value)
        startDrag evt id

dragHandler :: DragMoveHandler Global.State
dragHandler ds _ id = do
    enabled <- isEnabled id
    when enabled $ do
        startValue <- inRegistry $ UICmd.get id Model.dragStartValue
        withJust startValue $ \startValue -> do
            width <- inRegistry $ UICmd.get id $ Model.size . x
            let diff    = ds ^. currentPos - ds ^. startPos
                delta   = if (abs $ diff ^. x) > (abs $ diff ^. y) then  diff ^. x /  divider
                                                                   else -diff ^. y / (divider * 10.0)
                divider = width * (keyModMult $ ds ^. keyMods)
            inRegistry $ UICmd.update_ id $ Model.boundedNormValue .~ (startValue + delta)

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
    let shiftDown = evt ^. Mouse.keyMods . shift
    when (enabled && not shiftDown) $ do
        (tbId:_) <- inRegistry $ UICmd.children id
        UICmd.takeFocus tbId
        inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

clickHandler :: DblClickHandler Global.State
clickHandler evt _ id = do
    enabled <- isEnabled id
    let shiftDown = evt ^. Mouse.keyMods . shift
    when (enabled && shiftDown) $ do
        width <- inRegistry $ UICmd.get id $ Model.size . x
        let normValue = (evt ^. Mouse.position ^. x) / width
        widget <- inRegistry $ UICmd.update id $ Model.boundedNormValue .~ normValue
        triggerValueChanged (widget ^. Model.value) id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & click        .~ clickHandler
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
    let val' = rational val
    case val' of
        Left err        -> inRegistry $ do
            val <- UICmd.get parent Model.displayValue
            UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ val)
        Right (val', _) -> do
            inRegistry $ UICmd.update_ parent $ Model.value .~ val'
            triggerValueChanged val' parent


instance CompositeWidget Model.ContinuousSlider where
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

instance ResizableWidget Model.ContinuousSlider where resizeWidget = TextBox.labeledEditableResize
