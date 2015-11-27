module UI.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Types                         (JSVal, JSString)
import           GHCJS.Marshal.Pure                  (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text                  (lazyTextToJSString)

import           UI.Widget                           (UIWidget(..))
import qualified Object.Widget.Button             as Model
import qualified UI.Widget                        as Widget
import qualified UI.Registry                      as UI
import qualified UI.Generic                       as UI
import           Object.Widget
import           Object.UITypes

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
