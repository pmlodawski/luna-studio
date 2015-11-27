module UI.Widget.List where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Types        (JSVal, JSString)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text (lazyTextToJSString)

import           UI.Widget          (UIWidget(..))
import qualified Object.Widget.List  as Model
import qualified UI.Widget            as Widget
import qualified UI.Registry          as UI
import qualified UI.Generic           as UI
import           Object.Widget
import           Object.UITypes

newtype List = List JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget List

foreign import javascript unsafe "new Group($1)" create' :: Int    -> IO List

create :: WidgetId -> Model.List -> IO List
create oid model = do
    toggle      <- create' oid
    UI.setWidgetPosition (model ^. widgetPosition) toggle
    return toggle

instance UIDisplayObject Model.List where
    createUI parentId id model = do
        list    <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id list
        Widget.add list parent

    updateUI id old model = return ()
