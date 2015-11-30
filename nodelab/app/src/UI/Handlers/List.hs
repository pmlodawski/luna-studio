module UI.Handlers.List where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Object.UITypes

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (TypeKey(..))
import qualified Object.Widget.List as Model
import           UI.Widget.Toggle ()
import           UI.Generic (takeFocus, startDrag)

newtype ValueChangedHandler = ValueChangedHandler (WidgetId -> Command Global.State ())
valueChangedHandler = TypeKey :: TypeKey ValueChangedHandler

triggerValueChanged :: WidgetId -> Command Global.State ()
triggerValueChanged id = do
    maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandler
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler id
