module UI.Widget.Group where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Group           as Model
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget

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

    updateUI id old model = do
        whenChanged old model Model.size  $ UI.setSize id model

instance CompositeWidget Model.Group
