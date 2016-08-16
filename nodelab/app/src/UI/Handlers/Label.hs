module UI.Handlers.Label ( widgetHandlers
                         , ClickedHandler(..)
                         , DblClickedHandler(..)
                         ) where

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
import           UI.Handlers.Button           (ClickedHandler, DblClickedHandler, clickHandler, dblClickHandler, triggerMousePressed)

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click        .~ clickHandler
                     & dblClick     .~ dblClickHandler
                     & mouseOver    .~ (\_ _ -> performIO $ setCursor Pointer)
                     & mouseOut     .~ (\_ _ -> performIO $ setCursor Normal)
                     & mousePressed .~ triggerMousePressed

