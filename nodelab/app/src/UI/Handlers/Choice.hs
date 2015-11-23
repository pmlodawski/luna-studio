module UI.Handlers.Choice where

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
import qualified Object.Widget.Choice as Model
import           UI.Widget.Toggle ()
import           UI.Generic (takeFocus, startDrag)

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def
