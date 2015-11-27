module UI.Layout where

import           Utils.PreludePlus
import           Utils.Vector
import           Control.Monad (forM, foldM)

import           Object.UITypes
import           Object.Widget
import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.Commands.UIRegistry  as UICmd
import qualified UI.Command.Group              as Group

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

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getHeight widgets
    height  <- foldM (moveY spacing) 0.0 heights
    -- UIRegistry.widgets . ix id . widget . widgetSize . y .= height
    Group.updateSize id

    -- TODO: Update size on UI
    -- performIO $ updateUI id (oldWidget ^. widget) newWidget

horizontalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
horizontalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getWidth widgets
    height  <- foldM (moveX spacing) 0.0 heights
    Group.updateSize id
    -- UIRegistry.widgets . ix id . widget . widgetSize . x .= height

    -- TODO: Update size on UI
    -- performIO $ updateUI id (oldWidget ^. widget) newWidget
