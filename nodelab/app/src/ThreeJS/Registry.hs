{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Registry where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSRef)
import           Object.Widget
import qualified JavaScript.Object as JSObject
import qualified Data.JSString as JSString
import           ThreeJS.Types
import           ThreeJS.Uniform

foreign import javascript unsafe "$$.registry[$1]"
    getFromRegistryJS :: Int -> IO (JSRef a)

foreign import javascript unsafe "$$.registry[$1] = $2"
    putToRegistryJS :: Int -> JSRef a -> IO ()

foreign import javascript unsafe "delete $$.registry[$1]"
    removeFromRegistryJS :: Int -> IO ()

class (Object b) => UIWidget b where
    wrapWidget   :: JSObject.Object -> b
    unwrapWidget :: b               -> JSObject.Object

    buildSkeleton :: Mesh -> IO (b, UniformMap)
    buildSkeleton m = do
        widget     <- JSObject.create
        uniforms   <- buildUniformMap
        widgetMesh <- mesh m
        JSObject.setProp "mesh"       widgetMesh                                  widget
        JSObject.setProp "uniforms"   (JSObject.getJSRef $ unUniformMap uniforms) widget

        return (wrapWidget widget, uniforms)

class (IsDisplayObject a, UIWidget b) => UIWidgetBinding a b | a -> b where
    lookup       :: a               -> IO b
    lookup widget = (getFromRegistryJS oid >>= return . wrapWidget . JSObject.fromJSRef) where oid = objectId widget

    register     :: a -> b          -> IO ()
    register widget uiWidget = putToRegistryJS oid uiref where
        oid   = objectId widget
        uiref = (JSObject.getJSRef $ unwrapWidget uiWidget)

    updateUniformValue :: (Enum d, Show d) => d -> JSRef c -> a -> IO ()
    updateUniformValue n v w = do
        bref     <- ThreeJS.Registry.lookup w
        uniforms <- JSObject.getProp "uniforms"                         (unwrapWidget bref) >>= return .           JSObject.fromJSRef
        uniform  <- JSObject.getProp (JSString.pack $ locaseFirst $ show n)  uniforms       >>= return . Uniform . JSObject.fromJSRef
        setValue uniform v


unregister :: (IsDisplayObject a) => a -> IO ()
unregister widget  = removeFromRegistryJS $ objectId widget

