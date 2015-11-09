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
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap,TypeKey(..))

register :: DisplayObjectClass a => WidgetId -> a -> HTMap -> Command UIRegistry.State WidgetId
register parent model handlers = do
    file <- UIRegistry.registerM parent model handlers
    performIO $ createUI parent (file ^. objectId) model
    return (file ^. objectId)

update :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command UIRegistry.State a
update id fun = do
    oldWidget <- UIRegistry.lookupTypedM  id
    case oldWidget of
        Nothing        -> error $ "Widget " <> (show id) <> " not found!"
        Just oldWidget -> do
            newWidget  <- UIRegistry.updateWidgetM id fun
            performIO $ updateUI id (oldWidget ^. widget) newWidget
            return newWidget

update_ :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command UIRegistry.State ()
update_ id fun = update id fun >> return ()

move :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
move id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition .= vec
    performIO $ UI.updatePosition' id vec

moveBy :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
moveBy id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition += vec
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    forM_ pos $  performIO . (UI.updatePosition' id)

get :: DisplayObjectClass a => WidgetId -> Lens' a b -> Command UIRegistry.State b
get id f = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
    return $ (file ^. widget . f)

handler :: Typeable k => WidgetId -> TypeKey k -> Command UIRegistry.State (Maybe k)
handler id k = do
    maybeFile <- UIRegistry.lookupM id
    case maybeFile of
        Nothing   -> return Nothing
        Just file -> return $ HMap.lookup k (file ^. handlers)

removeWidget :: WidgetId -> Command UIRegistry.State ()
removeWidget id = do
    widgets <- UIRegistry.unregisterM id
    forM_ widgets $ performIO . UI.removeWidget
