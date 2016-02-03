{-# LANGUAGE Rank2Types #-}

module UI.Command.Group where

import           Control.Monad                (foldM, forM)
import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget                (WidgetId, widget, widgetPosition, widgetSize)
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler, sceneGraphId, sceneInterfaceId)
import qualified Reactive.State.UIRegistry    as UIRegistry


maximum' :: [Double] -> Double
maximum' [] = 0.0
maximum' xs = maximum xs

getFarEdge :: Getter (Vector2 Double) Double -> WidgetId -> Command UIRegistry.State Double
getFarEdge getter id = do
    offset <- UICmd.get' id $ widgetPosition . getter
    size   <- UICmd.get' id $ widgetSize     . getter
    return $ offset + size

updateSize :: Vector2 Double -> WidgetId -> Command UIRegistry.State ()
updateSize (Vector2 padX padY) id = do
    widgets <- UICmd.children id
    widths  <- mapM (getFarEdge x) widgets
    heights <- mapM (getFarEdge y) widgets

    UICmd.resize id $ Vector2 (padX + maximum' widths) (padY + maximum' heights)
