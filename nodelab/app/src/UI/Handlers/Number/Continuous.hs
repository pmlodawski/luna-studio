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
-- import           UI.Instances ()
import           Data.Text.Lazy.Read (rational)
import           Reactive.State.UIRegistry (addHandler)
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)
import           UI.Handlers.Generic           (triggerValueChanged)

import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import qualified Object.Widget.TextBox           as TextBox
import qualified UI.Handlers.TextBox             as TextBox
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled id = inRegistry $ UICmd.get id Model.enabled

bumpValue :: Double -> WidgetId -> Command Global.State ()
bumpValue amount id = do
    widget <- zoom Global.uiRegistry $ UICmd.update id (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) id

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
            inRegistry $ UICmd.update_ id $ Model.value .~ (startValue + delta)

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


instance CompositeWidget Model.ContinuousNumber where
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

