{-# LANGUAGE Rank2Types #-}
module UI.Layout where

import           Utils.PreludePlus

import           Control.Monad                (foldM, forM)
import           Utils.Vector

import           Object.Widget                (WidgetId, widgetSize)
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler, sceneGraphId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified UI.Command.Group             as Group
import qualified UI.Handlers.Group            as Group

getHeight id = do
    size <- UICmd.get' id widgetSize
    return $ (id, size ^. y)

getWidth id = do
    size <- UICmd.get' id widgetSize
    return $ (id, size ^. x)

moveY spacing offset (id, height) = do
    UICmd.moveY id offset
    return $ offset + spacing + height

moveX spacing offset (id, width) = do
    UICmd.moveX id offset
    return $ offset + spacing + width

verticalLayoutHandler padding spacing = addHandler (UICmd.ChildrenResizedHandler $ verticalLayoutHandler' padding spacing) mempty

verticalLayoutHandler' :: Vector2 Double -> Double -> WidgetId -> WidgetId -> Command UIRegistry.State ()
verticalLayoutHandler' padding spacing id _ = do
    verticalLayout (padding ^. y) spacing id
    Group.updateSize padding id

verticalLayout :: Double -> Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout padding spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getHeight widgets
    void $ foldM (moveY spacing) padding heights
    forM_ widgets $ flip UICmd.moveX padding

horizontalLayoutHandler padding spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' padding spacing) mempty

horizontalLayoutHandler' :: Vector2 Double -> Double -> WidgetId -> WidgetId -> Command UIRegistry.State ()
horizontalLayoutHandler' padding spacing id _ = do
    horizontalLayout (padding ^. x) spacing id
    Group.updateSize padding id

horizontalLayout :: Double -> Double -> WidgetId -> Command UIRegistry.State ()
horizontalLayout padding spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getWidth widgets
    void $ foldM (moveX spacing) 0.0 heights
    forM_ widgets $ flip UICmd.moveY padding

flexVerticalLayoutHandler   spacing = addHandler (Group.WidgetResizedHandler $ flexVerticalLayout   spacing) mempty
flexHorizontalLayoutHandler spacing = addHandler (Group.WidgetResizedHandler $ flexHorizontalLayout spacing) mempty

flexVerticalLayout   = flexLayout y
flexHorizontalLayout = flexLayout x

flexLayout :: Lens' (Vector2 Double) Double -> Double -> WidgetId -> Vector2 Double -> Command UIRegistry.State ()
flexLayout lens spacing id size = do
    let height = size ^. lens
    widgets <- UICmd.children id
    let newHeight = height / (fromIntegral $ length widgets)
    forM_ widgets $ \id -> UICmd.resizeNoCB id (lens .~ newHeight)
    verticalLayout def spacing id
