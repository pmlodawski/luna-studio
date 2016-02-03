module UI.Widget.Choice where

import           Utils.PreludePlus             hiding (Choice)
import           Utils.Vector

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Choice          as Model
import qualified Reactive.State.UIRegistry        as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget
import           UI.Widget.Group ()


instance UIDisplayObject Model.Choice where
    createUI parentId id model = createUI parentId id (Model.toGroup model)
    updateUI id old model = updateUI id (Model.toGroup old) (Model.toGroup model)
