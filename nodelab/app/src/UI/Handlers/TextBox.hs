module UI.Handlers.TextBox where

import           Utils.PreludePlus

import           Utils.Vector

import           Data.JSString.Text           (lazyTextFromJSString, lazyTextToJSString)
import           GHCJS.Types                  (JSString)

import           Event.Event                  (JSState)
import           Object.Widget                (WidgetId, ClickHandler, KeyDownHandler, UIHandlers, keyDown, dblClick)
import qualified Object.Widget.TextBox        as Model
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           UI.Generic                   (takeFocus)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.TextBox            ()

dblClickHandler :: ClickHandler Global.State
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
widgetHandlers = def & keyDown   .~ keyDownHandler
                     & dblClick  .~ dblClickHandler
