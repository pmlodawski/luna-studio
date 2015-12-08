module UI.Widget.Number.Continuous where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           Data.JSString.Text (lazyTextToJSString)
import qualified Data.JSString as JSString

import           Data.Text.Lazy (Text)
import           Utils.Vector
import qualified Object.Widget.Number.Continuous as Model
import           Object.Widget
import           Object.UITypes

import qualified UI.Widget as Widget
import qualified UI.Registry as UI
import qualified UI.Generic  as UI
import           UI.Widget.Number (Number, create', setValueLabel', setLabel', setFocus')

createNumber :: WidgetId -> Model.ContinuousNumber -> IO Number
createNumber oid model = do
    slider   <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel    model slider
    UI.setWidgetPosition (model ^. Model.position) slider
    return slider

-- setValueLabel :: Model.ContinuousNumber -> Number -> IO ()
-- setValueLabel model slider = setValueLabel' slider $ JSString.pack $ model ^. Model.displayValue

setLabel :: Model.ContinuousNumber -> Number -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

instance UIDisplayObject Model.ContinuousNumber where
    createUI parentId id model = do
        slider   <- createNumber id model
        -- setValueLabel  model slider
        parent <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id slider
        Widget.add slider parent

    updateUI id old model = do
        slider <- UI.lookup id :: IO Number
        setLabel       model slider
        -- setValueLabel  model slider
