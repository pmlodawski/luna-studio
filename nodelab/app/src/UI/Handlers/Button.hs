module UI.Handlers.Button where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Event.Mouse as Mouse
import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.Button as Model
import           UI.Widget.Toggle ()
import           UI.Generic (takeFocus, startDrag)

newtype ClickedHandler = ClickedHandler (WidgetId -> Command Global.State ())
clickedHandler = TypeKey :: TypeKey ClickedHandler

triggerClicked :: WidgetId -> Command Global.State ()
triggerClicked id = do
    maybeHandler <- inRegistry $ UICmd.handler id clickedHandler
    forM_ maybeHandler $ \(ClickedHandler handler) -> handler id

clickHandler :: ClickHandler Global.State
clickHandler _ _ id = triggerClicked id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click .~ clickHandler
