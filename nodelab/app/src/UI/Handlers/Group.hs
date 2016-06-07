module UI.Handlers.Group where

import           Utils.PreludePlus

import           Utils.Vector
import           Data.HMap.Lazy               (TypeKey (..))

import           Object.Widget                (WidgetId, ResizableWidget, resizeWidget)
import qualified Object.Widget.Group          as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)

newtype WidgetResizedHandler = WidgetResizedHandler (WidgetId -> Vector2 Double -> Command UIRegistry.State ())
triggerWidgetResized :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
triggerWidgetResized id vec = do
    let key = TypeKey :: TypeKey WidgetResizedHandler
    maybeHandler <- UICmd.handler id key
    withJust maybeHandler $ \(WidgetResizedHandler handler) -> handler id vec

instance ResizableWidget Model.Group where
    resizeWidget id vec model = do
        defaultResize id vec model
        triggerWidgetResized id vec
