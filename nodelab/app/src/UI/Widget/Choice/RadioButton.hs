module UI.Widget.Choice.RadioButton where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text               (lazyTextToJSString)
import           GHCJS.Marshal.Pure               (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                      (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Choice.RadioButton as Model
import           Object.Widget.CompositeWidget    (CompositeWidget, createWidget, updateWidget)

import qualified UI.Generic                       as UI
import qualified UI.Registry                      as UI
import           UI.Widget                        (UIWidget (..))
import qualified UI.Widget                        as Widget

newtype RadioButton = RadioButton JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget RadioButton

foreign import javascript unsafe "new RadioButton($1, $2, $3)" create'   :: Int         -> Double -> Double -> IO RadioButton
foreign import javascript unsafe "$1.setValue($2)"             setValue' :: RadioButton -> Bool             -> IO ()
foreign import javascript unsafe "$1.setLabel($2)"             setLabel' :: RadioButton -> JSString         -> IO ()

create :: WidgetId -> Model.RadioButton -> IO RadioButton
create oid model = do
    widget      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model widget
    setValue       model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.RadioButton -> RadioButton -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setValue :: Model.RadioButton -> RadioButton -> IO ()
setValue model widget = setValue' widget $ model ^. Model.selected

instance UIDisplayObject Model.RadioButton where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = do
        widget <- UI.lookup id :: IO RadioButton

        setLabel model widget
        setValue model widget

instance CompositeWidget Model.RadioButton where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

