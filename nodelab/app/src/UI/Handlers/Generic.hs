{-# LANGUAGE ScopedTypeVariables #-}

module UI.Handlers.Generic where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (TypeKey (..))
import           Utils.Vector

import           Object.Widget                (WidgetId)
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

newtype ValueChangedHandler a = ValueChangedHandler (a -> WidgetId -> Command Global.State ())

triggerValueChanged :: Typeable a => a -> WidgetId -> Command Global.State ()
triggerValueChanged new id = do
    let key = TypeKey :: (TypeKey (ValueChangedHandler a))
    maybeHandler <- inRegistry $ UICmd.handler id key
    forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id
