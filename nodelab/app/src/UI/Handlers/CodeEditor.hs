module UI.Handlers.CodeEditor where

import           Utils.PreludePlus

import           Utils.Vector

import           Data.JSString.Text           (lazyTextFromJSString)
import qualified Data.Text.Lazy               as Text
import           GHCJS.Types                  (JSString)

import           Event.Event                  (JSState)
import           Event.Widget                 (Payload (..))
import           Object.Widget                (ClickHandler, IsDisplayObject, KeyDownHandler, UIHandlers, WidgetId, dblClick, fromWidgetId,
                                               keyDown, widgetCustom)
import qualified Object.Widget.CodeEditor     as Model
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.CodeEditor         ()


foreign import javascript safe "$1.registry[$2].getCode()" getCode' :: JSState -> Int -> JSString


customHandler :: Payload -> WidgetId -> Command Global.State ()
customHandler CodeEditorBlur widgetId = do
    jsState <- use $ Global.jsState
    let value = lazyTextFromJSString $ getCode' jsState $ fromWidgetId widgetId
    inRegistry $ UICmd.update' widgetId $ Model.value .~ value
    triggerValueChanged value widgetId

customHandler _ _ = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & widgetCustom .~ customHandler
