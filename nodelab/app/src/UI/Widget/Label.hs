module UI.Widget.Label where

import           Utils.PreludePlus

import           Utils.Vector
import qualified Data.JSString                 as JSString
import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Label           as Model
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget

newtype Label = Label JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Label

foreign import javascript unsafe "new Label($1, $2, $3)" create'       :: Int -> Double -> Double -> IO Label
foreign import javascript unsafe "$1.setLabel($2)"       setLabel'     :: Label -> JSString -> IO ()
foreign import javascript unsafe "$1.setAlignment($2)"   setAlignment' :: Label -> JSString -> IO ()

create :: WidgetId -> Model.Label -> IO Label
create oid model = do
    widget      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model widget
    setAlignment   model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.Label -> Label -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setAlignment :: Model.Label -> Label -> IO ()
setAlignment model label = setAlignment' label $ JSString.pack $ show $ model ^. Model.alignment

instance UIDisplayObject Model.Label where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = do
        widget <- UI.lookup id :: IO Label
        setLabel     model widget
        setAlignment model widget

instance CompositeWidget Model.Label
instance ResizableWidget Model.Label where resizeWidget = UI.defaultResize
