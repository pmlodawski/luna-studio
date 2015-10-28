{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Types where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSVal)
import           Object.Widget
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

newtype Container = Container { unContainer :: JSVal } deriving (PFromJSVal, PToJSVal)
newtype Mesh      = Mesh      { unMesh      :: JSVal } deriving (PFromJSVal, PToJSVal)

class (PFromJSVal a, PToJSVal a) => UIWidget a

foreign import javascript unsafe "$1.mesh"      getMesh'      :: JSVal -> IO Mesh
foreign import javascript unsafe "$1.container" getContainer' :: JSVal -> IO Container
foreign import javascript unsafe "$2.add($1)"   add'          :: Mesh  -> Container -> IO ()

getMesh      :: UIWidget a => a -> IO Mesh
getMesh = getMesh' . pToJSVal

getContainer :: UIWidget a => a -> IO Container
getContainer = getContainer' . pToJSVal

add :: (UIWidget a, UIWidget b) => a -> b -> IO ()
add widget container = do
    mesh           <- getMesh widget
    containerMesh  <- getContainer container
    add' mesh containerMesh
