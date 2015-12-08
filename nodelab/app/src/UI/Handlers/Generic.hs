{-# LANGUAGE ScopedTypeVariables #-}

module UI.Handlers.Generic where

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

newtype ValueChangedHandler a = ValueChangedHandler (a -> WidgetId -> Command Global.State ())

triggerValueChanged :: Typeable a => a -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    let key = TypeKey :: (TypeKey (ValueChangedHandler a))
    maybeHandler <- inRegistry $ UICmd.handler id key
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id
