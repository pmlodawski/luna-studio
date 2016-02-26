{-# LANGUAGE Rank2Types #-}
module UI.Layout where

import           Utils.PreludePlus

import           Control.Monad                (foldM, forM)
import           Utils.Vector

import           Object.Widget                (WidgetId, widgetSize, widgetVisible)
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler, sceneGraphId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified Object.Widget.Group          as Group
import qualified UI.Command.Group             as Group
import qualified UI.Handlers.Group            as Group
import           UI.Widget.Group              ()

getHeight id = do
    size <- UICmd.get' id widgetSize
    vis <- UICmd.get' id widgetVisible
    return $ (id, size ^. y, vis)

getWidth id = do
    size <- UICmd.get' id widgetSize
    vis <- UICmd.get' id widgetVisible
    return $ (id, size ^. x, vis)

moveY spacing offset (id, height, True) = do
    UICmd.moveY id offset
    return $ offset + spacing + height
moveY spacing offset (id, height, False) = do
    return $ offset

moveX spacing offset (id, width, True) = do
    UICmd.moveX id offset
    return $ offset + spacing + width
moveX spacing offset (id, width, False) = do
    return $ offset

verticalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ verticalLayoutHandler' spacing) mempty

verticalLayoutHandler' :: Double -> WidgetId -> WidgetId -> Command UIRegistry.State ()
verticalLayoutHandler' spacing id _ = do
    verticalLayout spacing id
    maybePadding <- UICmd.maybeGet id $ Group.style . Group.padding
    let padding = fromMaybe def maybePadding
    Group.updateSize padding id

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getHeight widgets
    void $ foldM (moveY spacing) 0.0 heights

horizontalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' spacing True) mempty

horizontalLayoutHandlerNoResize spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' spacing False) mempty

horizontalLayoutHandler' :: Double -> Bool -> WidgetId -> WidgetId -> Command UIRegistry.State ()
horizontalLayoutHandler' spacing resize id _ = do
    horizontalLayout spacing id
    maybePadding <- UICmd.maybeGet id $ Group.style . Group.padding
    let padding = fromMaybe def maybePadding
    when resize $ Group.updateSize padding id

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
