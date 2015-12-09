module UI.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Button          as Model
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget

newtype Button = Button JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Button

foreign import javascript unsafe "new Button($1, $2, $3)" create'     :: Int         -> Double -> Double -> IO Button
foreign import javascript unsafe "$1.setLabel($2)"        setLabel'   :: Button      -> JSString         -> IO ()
foreign import javascript unsafe "$1.setEnabled($2)"      setEnabled' :: Button      -> Bool             -> IO ()

create :: WidgetId -> Model.Button -> IO Button
create oid model = do
    widget      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model widget
    setEnabled     model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.Button -> Button -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setEnabled :: Model.Button -> Button -> IO ()
setEnabled model widget = setEnabled' widget $ model ^. Model.enabled

instance UIDisplayObject Model.Button where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = do
        widget <- UI.lookup id :: IO Button
        setLabel   model widget
        setEnabled model widget


instance CompositeWidget Model.Button where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()
