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

getHeight id = do
    size <- UICmd.get' id widgetSize
    return $ (id, size ^. y)

move spacing offset (id, height) = do
    UICmd.moveY id offset
    return $ offset + spacing + height

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing id = do
    widgets <- UICmd.children id
    heights <- mapM getHeight widgets
    height  <- foldM (move spacing) 0.0 heights
    UIRegistry.widgets . ix id . widget . widgetSize . y .= height

    -- TODO: Update size on UI
    -- performIO $ updateUI id (oldWidget ^. widget) newWidget
