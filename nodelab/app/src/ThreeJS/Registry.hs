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
import           Object.UITypes

foreign import javascript unsafe "$$.registry[$1]"
    getFromRegistryJS :: Int -> IO (JSRef a)

foreign import javascript unsafe "$$.registry[$1] = $2"
    putToRegistryJS :: Int -> JSRef a -> IO ()

foreign import javascript unsafe "delete $$.registry[$1]"
    removeFromRegistryJS :: Int -> IO ()

class (Show a) => ComponentKey a

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
    addComponent :: (ComponentKey a) => b -> a -> JSRef c -> IO ()
    addComponent w k v = JSObject.setProp (JSString.pack $ show k) v (unwrapWidget w)
    addComponents :: (ComponentKey a) => b -> [(a, JSRef c)] -> IO ()
    addComponents w l = mapM_ (\(k,v) -> addComponent w k v) l

    readComponent :: (ComponentKey a) => a -> b -> IO (JSRef c)
    readComponent k w = JSObject.getProp (JSString.pack $ show k)  (unwrapWidget w)
    readContainer :: b -> IO Group
    readContainer w =  JSObject.getProp "mesh"  (unwrapWidget w) >>= return . Group

    lookup       :: WidgetId -> IO b
    lookup oid    = (getFromRegistryJS oid >>= return . wrapWidget . JSObject.fromJSRef)

    register     :: WidgetId  -> b          -> IO ()
    register oid uiWidget = putToRegistryJS oid uiref where
        uiref = (JSObject.getJSRef $ unwrapWidget uiWidget)


class (IsDisplayObject a, UIWidget b) => UIWidgetBinding a b | a -> b where
    build :: WidgetId -> a -> IO b


unregister :: WidgetId -> IO ()
unregister oid  = removeFromRegistryJS oid


updateUniformValue :: (UniformKey d) => d -> JSRef c -> WidgetId -> IO ()
updateUniformValue n v oid = do
    bref     <- getFromRegistryJS oid >>= return . JSObject.fromJSRef
    uniforms <- JSObject.getProp "uniforms"                          bref >>= return .           JSObject.fromJSRef
    uniform  <- JSObject.getProp (JSString.pack $ uniformName n)  uniforms >>= return . Uniform . JSObject.fromJSRef
    setValue uniform v
