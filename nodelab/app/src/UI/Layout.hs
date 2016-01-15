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

verticalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ verticalLayoutHandler' spacing) mempty

verticalLayoutHandler' :: Double -> WidgetId -> WidgetId -> Command UIRegistry.State ()
verticalLayoutHandler' spacing id _ = do
    verticalLayout spacing id
    Group.updateSize id

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getHeight widgets
    void $ foldM (moveY spacing) 0.0 heights

horizontalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' spacing) mempty

horizontalLayoutHandler' :: Double -> WidgetId -> WidgetId -> Command UIRegistry.State ()
horizontalLayoutHandler' spacing id _ = do
    horizontalLayout spacing id
    Group.updateSize id

horizontalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
horizontalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getWidth widgets
    void $ foldM (moveX spacing) 0.0 heights

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
    verticalLayout spacing id
