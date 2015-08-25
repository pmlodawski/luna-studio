{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FunctionalDependencies #-}

module ThreeJS.Registry where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSRef)
import           Object.Widget
import qualified JavaScript.Object as JSObject
import           ThreeJS.Types

foreign import javascript unsafe "$$.registry[$1]"
    getFromRegistryJS :: Int -> IO (JSRef a)

foreign import javascript unsafe "$$.registry[$1] = $2"
    putToRegistryJS :: Int -> JSRef a -> IO ()

foreign import javascript unsafe "delete $$.registry[$1]"
    removeFromRegistryJS :: Int -> IO ()

class (IsDisplayObject a, Object b) => UIWidget a b | a -> b where
    lookup     :: a      -> IO b
    register   :: a -> b -> IO ()

genericLookup :: (IsDisplayObject a) => (JSObject.Object -> b) -> a -> IO b
genericLookup wrapper widget = (getFromRegistryJS oid >>= return . wrapper . JSObject.fromJSRef) where
    oid = objectId widget

genericRegister :: (IsDisplayObject a) => (b -> JSObject.Object) -> a -> b -> IO ()
genericRegister unwrapper widget uiWidget = putToRegistryJS oid (JSObject.getJSRef $ unwrapper uiWidget) where
    oid = objectId widget

unregister :: (IsDisplayObject a) => a -> IO ()
unregister widget  = removeFromRegistryJS $ objectId widget
