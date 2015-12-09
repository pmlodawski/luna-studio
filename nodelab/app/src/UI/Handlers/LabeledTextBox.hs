module UI.Handlers.LabeledTextBox where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (TypeKey (..))
import           Utils.Vector

import           Data.JSString.Text           (lazyTextFromJSString, lazyTextToJSString)
import           GHCJS.Types                  (JSString)

import           Event.Event                  (JSState)
import qualified Event.Mouse                  as Mouse
import           Object.Widget                (DblClickHandler, KeyDownHandler, UIHandlers, WidgetId, dblClick, keyDown)
import qualified Object.Widget.LabeledTextBox as Model
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           UI.Generic                   (startDrag, takeFocus)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.LabeledTextBox     ()

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ id = do
    takeFocus undefined id
    inRegistry $ UICmd.update_ id $ Model.isEditing .~ True

foreign import javascript unsafe "$1.registry[$2].input.val()" getValue' :: JSState -> WidgetId -> JSString

keyDownHandler :: KeyDownHandler Global.State
keyDownHandler '\r' _ jsState id = do
    let value = lazyTextFromJSString $ getValue' jsState id
    inRegistry $ UICmd.update_ id $ (Model.isEditing .~ False)
                                  . (Model.value     .~ value)
    triggerValueChanged value id

keyDownHandler '\27' _ jsState id = do
    let value = lazyTextFromJSString $ getValue' jsState id
    inRegistry $ UICmd.update_ id $ Model.isEditing .~ False
keyDownHandler _ _ _ _ = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown  .~ keyDownHandler
                     & dblClick .~ dblClickHandler
