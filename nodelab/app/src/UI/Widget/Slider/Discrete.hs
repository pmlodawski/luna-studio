module UI.Widget.Slider.Discrete where

import           Utils.PreludePlus

import           GHCJS.Foreign
import qualified Data.JSString as JSString
import           Data.JSString.Text (lazyTextToJSString)

import           Data.Text.Lazy (Text)
import           Utils.Vector
import qualified Object.Widget.Slider.Discrete as Model
import           Object.Widget.Slider.Discrete (DiscreteSlider)
import           Object.Widget
import           Object.UITypes

import qualified UI.Widget as Widget
import qualified UI.Registry as UI
import qualified UI.Generic  as UI
import           UI.Widget.Slider (Slider, create', setValueLabel', setValue', setLabel', setFocus')
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

createSlider :: WidgetId -> Model.DiscreteSlider -> IO Slider
createSlider oid model = do
    slider      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    -- setValueLabel  model slider
    setValue       model slider
    UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

-- setValueLabel :: Model.DiscreteSlider -> Slider -> IO ()
-- setValueLabel model slider = setValueLabel' slider $ JSString.pack $ model ^. Model.displayValue

setLabel :: Model.DiscreteSlider -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.DiscreteSlider -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.boundedNormValue

instance UIDisplayObject Model.DiscreteSlider where
    createUI parentId id model = do
        slider   <- createSlider id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id slider
        Widget.add slider parent

    updateUI id old model = do
        slider <- UI.lookup id :: IO Slider

        setLabel       model slider
        -- setValueLabel  model slider
        setValue       model slider

instance CompositeWidget Model.DiscreteSlider where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

