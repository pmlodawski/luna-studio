{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Commands.UIRegistry where

import           Utils.PreludePlus hiding (children)
import           JS.Widget         as UI
import           Object.UITypes    (WidgetId)
import           Object.Widget
import           Utils.Vector

import qualified Reactive.State.UIRegistry as UIRegistry
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)
import           Reactive.Commands.Command (Command, performIO)
import qualified UI.Generic as UI
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap,TypeKey(..))


register :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State WidgetId
register parent model handlers = do
    file <- UIRegistry.registerM parent model handlers
    performIO $ createUI parent (file ^. objectId) model
    createWidget (file ^. objectId) model
    return (file ^. objectId)

register_ :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State ()
register_ parent model handlers = void $ register parent model handlers

update :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State a
update id fun = do
    oldWidget <- UIRegistry.lookupTypedM  id
    case oldWidget of
        Nothing        -> error $ "update: Widget " <> (show id) <> " not found or wrong type!"
        Just oldWidget -> do
            newWidget  <- UIRegistry.updateWidgetM id fun
            performIO $ updateUI id (oldWidget ^. widget) newWidget
            updateWidget id (oldWidget ^. widget) newWidget
            return newWidget

update_ :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State ()
update_ id fun = void $ update id fun

move :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
move id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition .= vec
    performIO $ UI.updatePosition' id vec

moveY :: WidgetId -> Double -> Command UIRegistry.State ()
moveY id ny = do
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    forM_ pos $ \(Vector2 px _) -> do
        UIRegistry.widgets . ix id . widget . widgetPosition . y .= ny
        let vec = Vector2 px ny
        performIO $ UI.updatePosition' id vec

moveX :: WidgetId -> Double -> Command UIRegistry.State ()
moveX id nx = do
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    forM_ pos $ \(Vector2 _ py) -> do
        UIRegistry.widgets . ix id . widget . widgetPosition . x .= nx
        let vec = Vector2 nx py
        performIO $ UI.updatePosition' id vec

moveBy :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
moveBy id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition += vec
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    forM_ pos $ performIO . (UI.updatePosition' id)

get :: DisplayObjectClass a => WidgetId -> Getter a b -> Command UIRegistry.State b
get id f = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file     = fromMaybe (error "get: invalid type or widget not exists") maybeFile
    return $ file ^. widget . f

get' :: WidgetId -> Getter DisplayObject b -> Command UIRegistry.State b
get' id f = do
    maybeFile <- UIRegistry.lookupM id
    let file     = fromMaybe (error $ "get': " <> (show id) <> " widget not exists") maybeFile
    return $ file ^. widget . f

lookup :: DisplayObjectClass a => WidgetId -> Command UIRegistry.State a
lookup id = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file   = fromMaybe (error "updateWidgetM: invalidType") maybeFile
    return $ file ^. widget

children :: WidgetId -> Command UIRegistry.State [WidgetId]
children id = do
    maybeFile <- UIRegistry.lookupM id
    let file   = fromMaybe (error "children: widget not exists") maybeFile
    return $ file ^. Object.Widget.children

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
