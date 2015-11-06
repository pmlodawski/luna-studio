{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Commands.UIRegistry where

import           Utils.PreludePlus
import           JS.Widget         as UI
import           Object.UITypes    (WidgetId)
import           Object.Widget
import           Utils.Vector

import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)
import qualified UI.Generic as UI


register :: DisplayObjectClass a => WidgetId -> a -> UIHandlers st -> Command (UIRegistry.State st) WidgetId
register parent model handlers = do
    file <- UIRegistry.registerM parent model handlers
    performIO $ createUI parent (file ^. objectId) model
    return (file ^. objectId)

update :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command (UIRegistry.State b) ()
update id fun = do
    maybeFile   <- UIRegistry.lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
        newWidget  = fun $ file ^. widget
    UIRegistry.updateM id newWidget
    performIO $ updateUI id (file ^. widget) newWidget

move :: WidgetId -> Vector2 Double -> Command (UIRegistry.State b) ()
move id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition .= vec
    performIO $ UI.updatePosition' id vec

moveBy :: WidgetId -> Vector2 Double -> Command (UIRegistry.State b) ()
moveBy id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition += vec
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    forM_ pos $  performIO . (UI.updatePosition' id)

get :: DisplayObjectClass a => WidgetId -> Lens' a b -> Command (UIRegistry.State st) b
get id f = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
    return $ (file ^. widget . f)
