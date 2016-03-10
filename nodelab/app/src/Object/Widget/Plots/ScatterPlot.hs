module Object.Widget.Plots.ScatterPlot where

import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget

data DisplayType = Bubbles | Bars deriving (Show, Eq, Generic)

data ScatterPlot = ScatterPlot { _position   :: Vector2 Double
                               , _size       :: Vector2 Double
                               , _dataPoints :: [Vector2 Double]
                               , _display    :: DisplayType
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ScatterPlot
instance ToJSON ScatterPlot

makeLenses ''DisplayType
instance ToJSON DisplayType

create :: Size -> ScatterPlot
create size = ScatterPlot def size def Bubbles

instance IsDisplayObject ScatterPlot where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
