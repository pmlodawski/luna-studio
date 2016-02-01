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

foreign import javascript unsafe "new ScatterPlot($1, $2, $3)" create'  :: Int         -> Double -> Double -> IO ScatterPlot
foreign import javascript unsafe "$1.setData($2)"              setData' :: ScatterPlot -> JSArray -> IO ()

foreign import javascript unsafe "{Index: $1, Value: $2}" createDataPoint' :: Double -> Double -> IO JSVal

setData :: ScatterPlot -> Model.ScatterPlot -> IO ()
setData plot model = do
    dataPoints <- mapM (uncurry createDataPoint') (model ^. Model.dataPoints)
    setData' plot $ JSArray.fromList dataPoints

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


