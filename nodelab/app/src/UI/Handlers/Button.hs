module UI.Handlers.Button where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (TypeKey (..))

import           Object.Widget                (ClickHandler, UIHandlers, WidgetId, click, mouseOver, mouseOut)
import qualified Object.Widget.Button         as Model
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           JS.Cursor                    (setCursor, Cursor(Normal, Pointer))
import           UI.Widget.Toggle             ()

newtype ClickedHandler = ClickedHandler (WidgetId -> Command Global.State ())
clickedHandler = TypeKey :: TypeKey ClickedHandler

triggerClicked :: WidgetId -> Command Global.State ()
triggerClicked id = do
    maybeHandler <- inRegistry $ UICmd.handler id clickedHandler
    forM_ maybeHandler $ \(ClickedHandler handler) -> handler id

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerClicked id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click     .~ clickHandler
                     & mouseOver .~ (\_ _ -> performIO $ setCursor Pointer)
                     & mouseOut  .~ (\_ _ -> performIO $ setCursor Normal)
