module UI.Handlers.TextBox where

import           Utils.PreludePlus

import           Utils.Vector

import           Data.JSString.Text           (lazyTextFromJSString, lazyTextToJSString)
import           GHCJS.Types                  (JSString)

import           Event.Event                  (JSState)
import           Object.Widget                (WidgetId, ClickHandler, KeyDownHandler, UIHandlers, keyDown, dblClick, widgetSize, IsDisplayObject)
import qualified Object.Widget.TextBox        as Model
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.Commands.Command    (Command, ioCommand, performIO)
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.TextBox            ()


dblClickHandler :: ClickHandler Global.State
dblClickHandler _ _ id = do
    UICmd.takeFocus id
    inRegistry $ UICmd.update_ id $ Model.isEditing .~ True

foreign import javascript safe "$1.registry[$2].input.val()" getValue' :: JSState -> WidgetId -> JSString

keyDownHandler :: KeyDownHandler Global.State
keyDownHandler '\r'  _ _ = applyChanges
keyDownHandler '\27' _ _ = abortChanges
keyDownHandler _     _ _ = const $ return ()

applyChanges :: WidgetId -> Command Global.State ()
applyChanges id = do
    jsState <- use $ Global.jsState
    let value = lazyTextFromJSString $ getValue' jsState id
    inRegistry $ UICmd.update_ id $ (Model.isEditing .~ False)
                                  . (Model.value     .~ value)
    triggerValueChanged value id

abortChanges :: WidgetId -> Command Global.State ()
abortChanges id = do
    jsState <- use $ Global.jsState
    let value = lazyTextFromJSString $ getValue' jsState id
    inRegistry $ UICmd.update_ id $ Model.isEditing .~ False

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown   .~ keyDownHandler
                     & dblClick  .~ dblClickHandler

-------------------------------------------------------------

labeledEditableResize :: IsDisplayObject a => WidgetId -> Vector2 Double -> a -> Command UIRegistry.State ()
labeledEditableResize id size model = do
    defaultResize id size model

    (tbId:_) <- UICmd.children id
    let tx      = (model ^. widgetSize . x) / 2.0
        ty      = (model ^. widgetSize . y)
        sx      = tx - (model ^. widgetSize . y / 2.0)
    UICmd.resize tbId $ Vector2 sx ty
    UICmd.moveX tbId tx
