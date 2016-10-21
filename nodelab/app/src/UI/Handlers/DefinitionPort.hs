module UI.Handlers.DefinitionPort where

import           Object.Widget         (UIHandlers)
import qualified Reactive.State.Global as Global
import           UI.Widget.Toggle      ()
import           Utils.PreludePlus



widgetHandlers :: UIHandlers Global.State
widgetHandlers = def
