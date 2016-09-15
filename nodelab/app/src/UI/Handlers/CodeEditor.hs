module UI.Handlers.CodeEditor where

import           Utils.PreludePlus

import           Utils.Vector

import           Data.JSString.Text           (lazyTextFromJSString)
import           GHCJS.Types                  (JSString)

import           Event.Event                  (JSState)
import           Object.Widget                (WidgetId, ClickHandler, KeyDownHandler, UIHandlers, keyDown, dblClick, widgetSize, IsDisplayObject)
import qualified Object.Widget.TextBox        as Model
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.Commands.Command    (Command)
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.CodeEditor         ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def
