module UI.Widget.Slider.Discrete where

import           Data.Text.Lazy                (Text)
import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.JSString                 as JSString
import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Foreign

import           Object.UITypes
import           Object.Widget
import           Object.Widget.Slider.Discrete (DiscreteSlider)
import qualified Object.Widget.Slider.Discrete as Model
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import qualified UI.Widget                     as Widget
import           UI.Widget.Slider              (Slider, create', setFocus', setLabel', setValue', setValueLabel', setTicks', limitTicks)

createSlider :: WidgetId -> Model.DiscreteSlider -> IO Slider
createSlider oid model = do
    slider      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    setValue       model slider
    setTicks       model slider
    UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

setLabel :: Model.DiscreteSlider -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.DiscreteSlider -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.boundedNormValue

setTicks :: Model.DiscreteSlider -> Slider -> IO ()
setTicks model slider = setTicks' slider True offset span where
    (offset, span) = limitTicks (model ^. Model.minValue) (model ^. Model.maxValue) (model ^. Model.size . x)

instance UIDisplayObject Model.DiscreteSlider where
    createUI parentId id model = do
        slider   <- createSlider id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id slider
        Widget.add slider parent

    updateUI id old model = do
        slider <- UI.lookup id :: IO Slider
        whenChanged old model Model.label $ setLabel model slider
        whenChanged old model Model.value $ setValue model slider
        whenChanged old model Model.range $ setTicks model slider
