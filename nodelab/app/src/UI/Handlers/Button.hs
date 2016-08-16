module UI.Handlers.Button where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (TypeKey (..))

import           Event.Event                  (JSState)
import qualified Event.Mouse                  as Mouse
import           Object.Widget                (ClickHandler, DblClickHandler, UIHandlers, WidgetId, click, dblClick,
                                               mouseOut, mouseOver, mousePressed)
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           JS.Cursor                    (Cursor (Normal, Pointer), setCursor)
import           UI.Widget.Toggle             ()

newtype ClickedHandler = ClickedHandler (WidgetId -> Command Global.State ())
clickedHandler = TypeKey :: TypeKey ClickedHandler

triggerClicked :: WidgetId -> Command Global.State ()
triggerClicked id = do
    maybeHandler <- inRegistry $ UICmd.handler id clickedHandler
    withJust maybeHandler $ \(ClickedHandler handler) -> handler id

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerClicked id

newtype DblClickedHandler = DblClickedHandler (WidgetId -> Command Global.State ())
dblClickedHandler = TypeKey :: TypeKey DblClickedHandler

triggerDblClicked :: WidgetId -> Command Global.State ()
triggerDblClicked id = do
    maybeHandler <- inRegistry $ UICmd.handler id clickedHandler
    withJust maybeHandler $ \(ClickedHandler handler) -> handler id

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ id = triggerDblClicked id


newtype MousePressedHandler = MousePressedHandler (Mouse.Event' -> WidgetId  -> Command Global.State ())
mousePressedHandler = TypeKey :: TypeKey MousePressedHandler

triggerMousePressed :: Mouse.Event' -> JSState -> WidgetId -> Command Global.State ()
triggerMousePressed evt _ id = do
    maybeHandler <- inRegistry $ UICmd.handler id mousePressedHandler
    withJust maybeHandler $ \(MousePressedHandler handler) -> handler evt id


widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click        .~ clickHandler
                     & dblClick     .~ dblClickHandler
                     & mouseOver    .~ (\_ _ -> performIO $ setCursor Pointer)
                     & mouseOut     .~ (\_ _ -> performIO $ setCursor Normal)
                     & mousePressed .~ triggerMousePressed

