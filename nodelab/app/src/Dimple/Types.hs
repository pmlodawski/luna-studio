module Dimple.Types where

import           Utils.PreludePlus
import           GHCJS.Types      ( JSRef, JSString )


data SVGRef
type SVG = JSRef SVGRef

data DataFrameRef
type DataFrame = JSRef DataFrameRef

data ChartRef
type Chart = JSRef ChartRef


data AxisRef
type Axis = JSRef AxisRef
data AxisPosition = X | Y | Z deriving (Show, Eq, Enum)
data AxisType = Linear | Logarithmic | Percentage | Category | Time deriving (Show, Eq)


data PlotRef
type Plot = JSRef PlotRef


data SeriesRef
type Series = JSRef SeriesRef

data D3ElementRef
type D3Element = JSRef D3ElementRef
