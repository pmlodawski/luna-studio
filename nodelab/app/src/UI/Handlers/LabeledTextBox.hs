module UI.Handlers.LabeledTextBox where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (HTMap)
import           Utils.Vector

import           Object.Widget                (CompositeWidget, DblClickHandler, ResizableWidget, UIHandlers, WidgetId,
                                               createWidget, dblClick, resizeWidget, updateWidget)
import qualified Object.Widget.LabeledTextBox as Model
import qualified Object.Widget.TextBox        as TextBox
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler)

import           UI.Generic                   (defaultResize)
import           UI.Widget.LabeledTextBox     ()

import           UI.Handlers.Generic          (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.TextBox          as TextBox

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ id = do
    (tbId:_) <- inRegistry $ UICmd.children id
    UICmd.takeFocus tbId
    inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & dblClick     .~ dblClickHandler

-- Constructors


textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (ValueChangedHandler $ textValueChangedHandler id)
                $ addHandler (UICmd.LostFocus $ lostFocusHandler id)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    inRegistry $ UICmd.update_ parent $ Model.value .~ val
    triggerValueChanged val parent

lostFocusHandler :: WidgetId -> WidgetId -> Command Global.State ()
lostFocusHandler _ id = TextBox.applyChanges id

instance CompositeWidget Model.LabeledTextBox where
    createWidget id model = do
        let tx      = (model ^. Model.size . x) / 2.0
            ty      = (model ^. Model.size . y)
            sx      = tx - (model ^. Model.size . y / 2.0)
            textVal = model ^. Model.value
            textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

        tbId <- UICmd.register id textBox $ textHandlers id
        UICmd.moveX tbId tx

    updateWidget id old model = do
        (tbId:_) <- UICmd.children id
        UICmd.update_ tbId $ TextBox.value .~ (model ^. Model.value)

instance ResizableWidget Model.LabeledTextBox where
    resizeWidget id size model = do
        defaultResize id size model

        (tbId:_) <- UICmd.children id
        let tx      = (model ^. Model.size . x) / 2.0
            ty      = (model ^. Model.size . y)
            sx      = tx - (model ^. Model.size . y / 2.0)
        UICmd.resize tbId $ Vector2 sx ty
        UICmd.moveX tbId tx
