module UI.Widget.Slider.Continuous where

import           Utils.PreludePlus
import           Data.Text.Lazy                  (Text)
import           Utils.Vector

import qualified Data.JSString                   as JSString
import           Data.JSString.Text              (lazyTextToJSString)
import           GHCJS.Foreign

import           Object.UITypes
import           Object.Widget
import           Object.Widget.Slider.Continuous (ContinuousSlider)
import qualified Object.Widget.Slider.Continuous as Model
import qualified Reactive.State.UIRegistry       as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                      as UI
import qualified UI.Registry                     as UI
import qualified UI.Widget                       as Widget
import           UI.Widget.Slider                (Slider, create', setFocus', setLabel', setValue', setValueLabel')

createSlider :: WidgetId -> Model.ContinuousSlider -> IO Slider
createSlider oid model = do
    slider      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    setValue       model slider
    UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

setLabel :: Model.ContinuousSlider -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.ContinuousSlider -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.boundedNormValue

instance UIDisplayObject Model.ContinuousSlider where
    createUI parentId id model = do
        slider   <- createSlider id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id slider
        Widget.add slider parent

    updateUI id old model = do
        slider <- UI.lookup id :: IO Slider
        whenChanged old model Model.label $ setLabel model slider
        whenChanged old model Model.value $ setValue model slider
