module UI.Widget.Group where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Types        (JSVal, JSString)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text (lazyTextToJSString)

import           UI.Widget          (UIWidget(..))
import qualified Object.Widget.Group  as Model
import qualified UI.Widget            as Widget
import qualified UI.Registry          as UI
import qualified UI.Generic           as UI
import           Object.Widget
import           Object.UITypes
import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

newtype Group = Group JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Group

foreign import javascript unsafe "new Group($1)" create' :: Int    -> IO Group

create :: WidgetId -> Model.Group -> IO Group
create oid model = do
    toggle      <- create' oid
    UI.setWidgetPosition (model ^. widgetPosition) toggle
    return toggle

instance UIDisplayObject Model.Group where
    createUI parentId id model = do
        group    <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id group
        Widget.add group parent

    updateUI id old model = return ()

instance CompositeWidget Model.Group where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

