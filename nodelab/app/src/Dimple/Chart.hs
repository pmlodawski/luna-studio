{-# LANGUAGE OverloadedStrings #-}

module Dimple.Chart where

import           Utils.PreludePlus
import           Utils.Vector
import           GHCJS.Foreign
import           Data.JSString.Text  (lazyTextToJSString)
import           GHCJS.DOM.Element   (Element, IsElement)
import           GHCJS.Types         (JSRef, JSString)
import           JavaScript.Array    (JSArray)

import           Dimple.Types


foreign import javascript safe "$r = new dimple.chart($1, $2)"    createChart              :: D3Element -> JSArray -> IO Chart

foreign import javascript safe "$5.setBounds($1, $2, $3, $4)"     setBoundsJS              :: Int -> Int -> Int -> Int -> Chart -> IO ()
setBounds :: Vector2 Int -> Vector2 Int -> Chart -> IO ()
setBounds (Vector2 x y) (Vector2 w h) chart = setBoundsJS x y w h chart

foreign import javascript safe "$5.setMargins($1, $2, $3, $4)"    setMargins               :: Int -> Int -> Int -> Int -> Chart -> IO ()


foreign import javascript safe "$3.addMeasureAxis($1, $2)"        addLinearMeasureAxisJS   :: JSString -> JSString -> Chart -> IO Axis
foreign import javascript safe "$3.addLogAxis($1, $2)"            addLogMeasureAxisJS      :: JSString -> JSString -> Chart -> IO Axis
foreign import javascript safe "$3.addPctAxis($1, $2)"            addPctMeasureAxisJS      :: JSString -> JSString -> Chart -> IO Axis
foreign import javascript safe "$3.addCategoryAxis($1, $2)"       addCategoryMeasureAxisJS :: JSString -> JSString -> Chart -> IO Axis
foreign import javascript safe "$3.addTimeAxis($1, $2)"           addTimeMeasureAxisJS     :: JSString -> JSString -> Chart -> IO Axis

addAxis :: AxisType -> AxisPosition -> Text -> Chart -> IO Axis
addAxis tpe position measure chart = fun pos (lazyTextToJSString measure) chart where
    pos = case position of
        X -> "x"
        Y -> "y"
        Z -> "z"
    fun = case tpe of
        Linear      -> addLinearMeasureAxisJS
        Logarithmic -> addLogMeasureAxisJS
        Percentage  -> addPctMeasureAxisJS
        Category    -> addCategoryMeasureAxisJS
        Time        -> addTimeMeasureAxisJS

foreign import javascript safe "$2.addSeries(null, $1)"           addSimpleSeries         :: Plot -> Chart -> IO Series
foreign import javascript safe "$3.addSeries($1, $2)"             addGroupedSeriesJS      :: JSString -> Plot -> Chart -> IO Series

addGroupedSeries :: Text -> Plot -> Chart -> IO Series
addGroupedSeries t = addGroupedSeriesJS (lazyTextToJSString t)

foreign import javascript safe "$1.draw()" draw :: Chart -> IO ()

-- foreign import javascript safe "$1.chart.addAxis(position, categoryFields, measure, timeField)
-- foreign import javascript safe "$1.chart.addColorAxis(measure, colors)
-- foreign import javascript safe "$1.chart.addLegend(x, y, width, height, horizontalAlign, series)
-- foreign import javascript safe "$1.chart.assignClass(tag, css)
-- foreign import javascript safe "$1.chart.assignColor(tag, fill, stroke, opacity)
-- foreign import javascript safe "$1.chart.getClass(tag)
-- foreign import javascript safe "$1.chart.getColor(tag)
-- foreign import javascript safe "$1.chart.setStoryboard(categoryFields, tickHandler)
