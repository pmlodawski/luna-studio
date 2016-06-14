{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Commands.UIRegistry
    ( LostFocus(..)
    , ChildrenResizedHandler(..)
    , children
    , get
    , get'
    , handler
    , lookup
    , maybeGet
    , move
    , moveBy
    , moveX
    , moveY
    , parent
    , removeWidget
    , resize
    , resize'
    , resizeNoCB
    , register
    , register_
    , takeFocus
    , triggerChildrenResized
    , update
    , update_
    ) where

import           Data.HMap.Lazy            (HTMap, TypeKey (..))
import qualified Data.HMap.Lazy            as HMap
import           Utils.PreludePlus         hiding (children, lookup)
import           Utils.Vector

import qualified JS.Cursor                 as Cursor
import           Object.Widget             hiding (children, parent)
import qualified Object.Widget
import           Reactive.Commands.Command (Command, performIO)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified UI.Generic                as UI

register :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State WidgetId
register parent model handlers = do
    file <- UIRegistry.registerM parent model handlers
    performIO $ createUI parent (file ^. objectId) model
    createWidget (file ^. objectId) model
    triggerChildrenResized parent (file ^. objectId)
    return (file ^. objectId)

register_ :: (CompositeWidget a, DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State ()
register_ parent model handlers = void $ register parent model handlers

update :: (Eq a, CompositeWidget a, DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State a
update id fun = do
    oldWidget <- UIRegistry.lookupTypedM  id
    case oldWidget of
        Nothing        -> error $ "update: Widget " <> (show id) <> " not found or wrong type!"
        Just oldWidget -> do
            newWidget  <- UIRegistry.updateWidgetM id fun
            when ((oldWidget ^. widget) /= newWidget) $ do
                performIO $ updateUI id (oldWidget ^. widget) newWidget
                updateWidget id (oldWidget ^. widget) newWidget
            return newWidget

update_ :: (Eq a, CompositeWidget a, DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State ()
update_ id fun = void $ update id fun

move :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
move id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition .= vec
    performIO $ UI.updatePosition' id vec

moveY :: WidgetId -> Double -> Command UIRegistry.State ()
moveY id ny = do
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    withJust pos $ \(Vector2 px _) -> do
        UIRegistry.widgets . ix id . widget . widgetPosition . y .= ny
        let vec = Vector2 px ny
        performIO $ UI.updatePosition' id vec

moveX :: WidgetId -> Double -> Command UIRegistry.State ()
moveX id nx = do
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    withJust pos $ \(Vector2 _ py) -> do
        UIRegistry.widgets . ix id . widget . widgetPosition . x .= nx
        let vec = Vector2 nx py
        performIO $ UI.updatePosition' id vec

moveBy :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
moveBy id vec = do
    UIRegistry.widgets . ix id . widget . widgetPosition += vec
    pos <- preuse $ UIRegistry.widgets . ix id . widget . widgetPosition
    withJust pos $ performIO . (UI.updatePosition' id)


newtype ChildrenResizedHandler = ChildrenResizedHandler (WidgetId -> WidgetId -> Command UIRegistry.State ())
triggerChildrenResized :: WidgetId -> WidgetId -> Command UIRegistry.State ()
triggerChildrenResized id child = do
    let key = TypeKey :: TypeKey ChildrenResizedHandler
    maybeHandler <- handler id key
    withJust maybeHandler $ \(ChildrenResizedHandler handler) -> handler id child


resize :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
resize id vec = resize' id (\_ -> vec)

resize' :: WidgetId -> (Vector2 Double -> Vector2 Double) -> Command UIRegistry.State ()
resize' = resize'CB True

resizeNoCB :: WidgetId -> (Vector2 Double -> Vector2 Double) -> Command UIRegistry.State ()
resizeNoCB = resize'CB False

resize'CB :: Bool -> WidgetId -> (Vector2 Double -> Vector2 Double) -> Command UIRegistry.State ()
resize'CB cb id f = do
    UIRegistry.widgets . ix id . widget . widgetSize %= f
    widgetFile <- preuse $ UIRegistry.widgets . ix id
    withJust widgetFile $ \widgetFile -> do
        let model   = widgetFile ^. widget
            wParent = widgetFile ^. Object.Widget.parent
        resizeWidget id (model ^. widgetSize) model
        when cb $ withJust wParent $ flip triggerChildrenResized id

get :: DisplayObjectClass a => WidgetId -> Getter a b -> Command UIRegistry.State b
get id f = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file     = fromMaybe (error $ "get: invalid type or widget " <> (show id) <> "  not exists") maybeFile
    return $ file ^. widget . f

maybeGet :: DisplayObjectClass a => WidgetId -> Getter a b -> Command UIRegistry.State (Maybe b)
maybeGet id f = do
    maybeFile <- UIRegistry.lookupTypedM id
    return $ (view $ widget . f) <$> maybeFile


get' :: WidgetId -> Getter DisplayObject b -> Command UIRegistry.State b
get' id f = do
    maybeFile <- UIRegistry.lookupM id
    let file     = fromMaybe (error $ "get': " <> (show id) <> " widget not exists") maybeFile
    return $ file ^. widget . f

lookup :: DisplayObjectClass a => WidgetId -> Command UIRegistry.State a
lookup id = do
    maybeFile <- UIRegistry.lookupTypedM id
    let file   = fromMaybe (error $ "lookup: " <> (show id) <> "  invalidType") maybeFile
    return $ file ^. widget

children :: WidgetId -> Command UIRegistry.State [WidgetId]
children id = do
    maybeFile <- UIRegistry.lookupM id
    let file   = fromMaybe (error $ "children: " <> (show id) <> " widget not exists") maybeFile
    return $ file ^. Object.Widget.children

parent :: WidgetId -> Command UIRegistry.State WidgetId
parent id = do
    maybeFile <- UIRegistry.lookupM id
    let file   = fromMaybe (error $ "parent: widget " <> (show id) <> " not exists") maybeFile
    return $ fromMaybe (error "parent: called on scene") $ file ^. Object.Widget.parent

handler :: Typeable k => WidgetId -> TypeKey k -> Command UIRegistry.State (Maybe k)
handler id k = do
    maybeFile <- UIRegistry.lookupM id
    case maybeFile of
        Nothing   -> return Nothing
        Just file -> return $ HMap.lookup k (file ^. handlers)

removeWidget :: WidgetId -> Command UIRegistry.State ()
removeWidget id = do
    widgetOver <- use $ UIRegistry.widgetOver
    widgets <- UIRegistry.unregisterM id
    when (elem id widgets) $ performIO $ Cursor.setCursor Cursor.Normal
    forM_ widgets $ performIO . UI.removeWidget

newtype LostFocus = LostFocus (WidgetId -> Command Global.State ())
triggerLostFocus :: WidgetId -> Command Global.State ()
triggerLostFocus id = do
    let key = TypeKey :: (TypeKey LostFocus)
    maybeHandler <- Global.inRegistry $ handler id key
    withJust maybeHandler $ \(LostFocus handler) -> handler id


takeFocus :: WidgetId -> Command Global.State ()
takeFocus id = do
    currentFocused <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    when (currentFocused /= Just id) $ do
        withJust currentFocused triggerLostFocus
        Global.uiRegistry . UIRegistry.focusedWidget ?= id
