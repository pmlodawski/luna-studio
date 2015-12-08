module UI.Widget.Label where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Types                         (JSVal, JSString)
import           GHCJS.Marshal.Pure                  (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text                  (lazyTextToJSString)

import           UI.Widget                           (UIWidget(..))
import qualified Object.Widget.Label              as Model
import qualified UI.Widget                        as Widget
import qualified UI.Registry                      as UI
import qualified UI.Generic                       as UI
import           Object.Widget
import           Object.UITypes
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

newtype Label = Label JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Label

foreign import javascript unsafe "new Label($1)"   create'     :: Int   -> IO Label
foreign import javascript unsafe "$1.setLabel($2)" setLabel'   :: Label -> JSString -> IO ()

create :: WidgetId -> Model.Label -> IO Label
create oid model = do
    widget      <- create' oid
    setLabel       model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.Label -> Label -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label


instance UIDisplayObject Model.Label where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = do
        widget <- UI.lookup id :: IO Label
        setLabel   model widget

instance CompositeWidget Model.Label where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

