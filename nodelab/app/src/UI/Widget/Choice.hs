module UI.Widget.Choice where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector

import           GHCJS.Types                         (JSVal, JSString)
import           GHCJS.Marshal.Pure                  (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text                  (lazyTextToJSString)

import           UI.Widget                           (UIWidget(..))
import qualified Object.Widget.Choice as Model
import qualified UI.Widget            as Widget
import qualified UI.Registry          as UI
import qualified UI.Generic           as UI
import           Object.Widget
import           Object.UITypes
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

newtype Choice = Choice JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Choice

foreign import javascript unsafe "new Choice($1, $2, $3)" create'   :: Int -> Double -> Double -> IO Choice

create :: WidgetId -> Model.Choice -> IO Choice
create oid model = do
    widget      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

instance UIDisplayObject Model.Choice where
    createUI parentId id model = do
        widget   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id widget
        Widget.add widget parent

    updateUI id old model = return ()

instance CompositeWidget Model.Choice where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

