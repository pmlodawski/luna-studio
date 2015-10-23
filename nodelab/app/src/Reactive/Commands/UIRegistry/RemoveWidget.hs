module Reactive.Commands.UIRegistry.RemoveWidget where

import           Utils.PreludePlus
import           JS.Widget         as UI
import           Object.UITypes    (WidgetId)

import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)

removeWidgets :: [WidgetId] -> Command (UIRegistry.State a) ()
removeWidgets ids = do
    idsToRemove <- UIRegistry.unregisterAllM ids
    performIO $ sequence_ $ UI.removeWidget <$> idsToRemove

removeWidget :: WidgetId -> Command (UIRegistry.State a) ()
removeWidget id = removeWidgets [id]
