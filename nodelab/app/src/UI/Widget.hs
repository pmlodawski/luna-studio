{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widget where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           Utils.Vector
import           GHCJS.Types         (JSVal)
import           Object.Widget
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

import           Reactive.Commands.Command (Command, ioCommand, performIO)
import qualified Reactive.State.UIRegistry as UIRegistry


newtype Container = Container { unContainer :: JSVal } deriving (PFromJSVal, PToJSVal)
newtype Mesh      = Mesh      { unMesh      :: JSVal } deriving (PFromJSVal, PToJSVal)

class (PFromJSVal a, PToJSVal a) => UIWidget a
class (UIWidget a) => UIContainer a

foreign import javascript unsafe "$1.mesh"      getMesh'      :: JSVal -> IO Mesh
foreign import javascript unsafe "$1.container" getContainer' :: JSVal -> IO Container
foreign import javascript unsafe "$2.add($1)"   add'          :: Mesh  -> Container -> IO ()

getMesh      :: UIWidget a => a -> IO Mesh
getMesh = getMesh' . pToJSVal

getContainer :: UIContainer a => a -> IO Container
getContainer = getContainer' . pToJSVal

add :: (UIWidget a, UIContainer b) => a -> b -> IO ()
add widget container = do
    mesh           <- getMesh widget
    containerMesh  <- getContainer container
    add' mesh containerMesh
