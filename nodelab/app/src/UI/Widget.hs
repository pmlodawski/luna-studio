module UI.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.JSString             as JSString
import           GHCJS.Foreign
import           GHCJS.Marshal.Pure        (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types               (JSVal)

import           Object.UITypes
import           Object.Widget
import           Reactive.Commands.Command (Command, ioCommand, performIO)

newtype Container = Container { unContainer :: JSVal } deriving (PFromJSVal, PToJSVal)
newtype Mesh      = Mesh      { unMesh :: JSVal } deriving (PFromJSVal, PToJSVal)

class (PFromJSVal a, PToJSVal a) => UIWidget a
class (UIWidget a) => UIContainer a

foreign import javascript unsafe "$1.mesh"      getMesh'      :: JSVal -> IO Mesh
foreign import javascript unsafe "$1.container" getContainer' :: JSVal -> IO Container
foreign import javascript unsafe "$2.add($1)"   add'          :: Mesh  -> Container -> IO ()

newtype GenericWidget = GenericWidget { unGenericWidget :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    GenericWidget
instance UIContainer GenericWidget

getMesh      :: UIWidget a => a -> IO Mesh
getMesh = getMesh' . pToJSVal

getContainer :: UIContainer a => a -> IO Container
getContainer = getContainer' . pToJSVal

add :: (UIWidget a, UIContainer b) => a -> b -> IO ()
add widget container = do
    mesh           <- getMesh widget
    containerMesh  <- getContainer container
    add' mesh containerMesh
