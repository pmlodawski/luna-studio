module UI.Widget.DefinitionPort where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text           (lazyTextToJSString)
import           GHCJS.Marshal.Pure           (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                  (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.DefinitionPort as Model

import           UI.Generic                   (whenChanged)
import qualified UI.Generic                   as UI
import qualified UI.Registry                  as UI
import           UI.Widget                    (UIWidget)
import qualified UI.Widget                    as Widget


newtype DefinitionPort = DefinitionPort JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget DefinitionPort

foreign import javascript safe "new DefinitionPort($1, $2, $3)" create'   :: Int         -> Double -> Double -> IO DefinitionPort
foreign import javascript safe "$1.setValue($2)"             setValue' :: DefinitionPort -> Bool             -> IO ()
foreign import javascript safe "$1.setLabel($2)"             setLabel' :: DefinitionPort -> JSString         -> IO ()

create :: WidgetId -> Model.DefinitionPort -> IO DefinitionPort
create oid model = do
    widget      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model widget
    setValue       model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.DefinitionPort -> DefinitionPort -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setValue :: Model.DefinitionPort -> DefinitionPort -> IO ()
setValue model widget = setValue' widget $ model ^. Model.selected

instance UIDisplayObject Model.DefinitionPort where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = do
        widget <- UI.lookup id :: IO DefinitionPort

        whenChanged old model Model.label    $ setLabel model widget
        whenChanged old model Model.selected $ setValue model widget

instance CompositeWidget Model.DefinitionPort
instance ResizableWidget Model.DefinitionPort where resizeWidget = UI.defaultResize
