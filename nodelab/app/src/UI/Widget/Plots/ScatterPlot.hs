module UI.Widget.Plots.ScatterPlot where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Marshal.Pure              (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                     (JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Plots.ScatterPlot as Model

import qualified UI.Generic                      as UI
import qualified UI.Registry                     as UI
import           UI.Widget                       (UIWidget)
import qualified UI.Widget                       as Widget

newtype ScatterPlot = ScatterPlot JSVal deriving (PToJSVal, PFromJSVal)


instance UIWidget ScatterPlot

foreign import javascript safe "new ScatterPlot($1, $2, $3)" create'  :: Int         -> Double -> Double -> IO ScatterPlot
foreign import javascript safe "$1.setData($2, $3)"              setData' :: ScatterPlot -> JSVal -> Bool -> IO ()

foreign import javascript safe "$1.push({Index: $2, Value: $3})" createDataPoint' :: JSVal -> Double -> Double -> IO JSVal
foreign import javascript safe "[]" emptyArr' :: IO JSVal

setData :: ScatterPlot -> Model.ScatterPlot -> IO ()
setData plot model = do
    outArr <- emptyArr'
    let plotData = (model ^. Model.dataPoints)
    seq plotData $ return ()
    forM_ plotData $ \v -> createDataPoint' outArr (v ^. x) (v ^. y)
    setData' plot outArr (model ^. Model.display == Model.Bars)

create :: WidgetId -> Model.ScatterPlot -> IO ScatterPlot
create oid model = do
    plot <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setData plot model
    UI.setWidgetPosition (model ^. widgetPosition) plot
    return plot

instance UIDisplayObject Model.ScatterPlot where
    createUI parentId id model = do
        plot   <- create id model
        parent <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id plot
        Widget.add plot parent

    updateUI id _ model = do
        plot <- UI.lookup id :: IO ScatterPlot
        setData plot model

instance CompositeWidget Model.ScatterPlot
instance ResizableWidget Model.ScatterPlot


