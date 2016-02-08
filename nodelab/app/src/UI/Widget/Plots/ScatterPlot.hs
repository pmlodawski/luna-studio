module UI.Widget.Plots.ScatterPlot where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Plots.ScatterPlot as Model
import qualified Reactive.State.UIRegistry       as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget
import           JavaScript.Array              (JSArray)
import qualified JavaScript.Array              as JSArray

newtype ScatterPlot = ScatterPlot JSVal deriving (PToJSVal, PFromJSVal)


instance UIWidget ScatterPlot

foreign import javascript safe "new ScatterPlot($1, $2, $3)" create'  :: Int         -> Double -> Double -> IO ScatterPlot
foreign import javascript safe "$1.setData($2)"              setData' :: ScatterPlot -> JSVal -> IO ()

foreign import javascript safe "$1.push({Index: $2, Value: $3})" createDataPoint' :: JSVal -> Double -> Double -> IO JSVal
foreign import javascript safe "[]" emptyArr' :: IO JSVal

setData :: ScatterPlot -> Model.ScatterPlot -> IO ()
setData plot model = do
    outArr <- emptyArr'
    let plotData = (model ^. Model.dataPoints)
    seq plotData $ return ()
    forM_ plotData $ \v -> createDataPoint' outArr (v ^. x) (v ^. y)
    setData' plot outArr

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

    updateUI id old model = do
        plot <- UI.lookup id :: IO ScatterPlot
        setData plot model

instance CompositeWidget Model.ScatterPlot
instance ResizableWidget Model.ScatterPlot


