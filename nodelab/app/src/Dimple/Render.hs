module Dimple.Render where

import           Utils.PreludePlus
import qualified Object.Widget.Chart as Widget
import           GHCJS.DOM (currentDocument)
import           GHCJS.DOM.Document (getElementById, createElementNS)
import           GHCJS.DOM.Element   (Element, IsElement, setAttribute, getStyle)
import           GHCJS.DOM.CSSStyleDeclaration   (setProperty)
import           GHCJS.DOM.Node (appendChild)
import           Data.JSString (JSString)
import qualified Data.JSString as JSString
import           JavaScript.Array         (JSArray)
import           Dimple.Types
import           Dimple.Chart
import qualified Dimple.Plot as Plot
import           Utils.Vector

convertAxisType :: Widget.AxisType -> AxisType
convertAxisType Widget.Linear      = Linear
convertAxisType Widget.Logarithmic = Logarithmic
convertAxisType Widget.Percentage  = Percentage
convertAxisType Widget.Category    = Category
convertAxisType Widget.Time        = Time

convertChartType :: Widget.ChartType -> IO Plot
convertChartType Widget.Line    = Plot.line
convertChartType Widget.Bar     = Plot.bar
convertChartType Widget.Scatter = Plot.bubble
convertChartType Widget.Area    = Plot.area


foreign import javascript unsafe "d3.select($1)" wrapD3 :: Element -> IO D3Element
foreign import javascript unsafe "common.chart = $1" setChart :: Chart -> IO ()

foreign import javascript unsafe "require('exampleData')" getExampleData :: IO JSArray


setStyle :: Element -> [(String, String)] -> IO ()
setStyle el styles = do
    Just style <- getStyle el
    forM_ styles (\(p,v) -> setProperty style p (Just v) "")

toPx :: (Show a, Num a) => a -> String
toPx n = (show n) <> "px"

displayChart :: Widget.Chart -> IO ()
displayChart chart = do
    Just document  <- currentDocument
    Just svg       <- createElementNS document (Just "http://www.w3.org/2000/svg") (Just "svg")
    Just container <- getElementById document "htmlcanvas"

    setStyle svg [ ("position", "absolute"                     )
                 , ("width",    toPx $ chart ^. Widget.size . x)
                 , ("height",   toPx $ chart ^. Widget.size . y)
                 , ("left",     toPx $ chart ^. Widget.position  . x)
                 , ("top",      toPx $ chart ^. Widget.position  . y)
                 ]

    appendChild container (Just svg)
    jsdata   <- getExampleData

    d3svg <- wrapD3 svg

    jschart  <- createChart d3svg jsdata
    setChart jschart
    setMargins 40 30 40 60 jschart

    xAxis    <- addAxis (convertAxisType $ chart ^. Widget.xScale) X (chart ^. Widget.xMeasure) jschart
    yAxis    <- addAxis (convertAxisType $ chart ^. Widget.yScale) Y (chart ^. Widget.yMeasure) jschart
    tpe      <- convertChartType $ chart ^. Widget.tpe
    series   <- addSimpleSeries tpe jschart

    void $ draw jschart



