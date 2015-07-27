{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Types where

import           Utils.PreludePlus
import qualified GHCJS.Prim.Internal.Build as Build

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array as JSArray

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import Unsafe.Coerce
import System.IO.Unsafe
import Debug.Trace

import GHCJS.Prim
import ThreeJS.Converters

class Container a where
    add    :: (Object b) => a -> b -> IO ()
    remove :: (Object b) => a -> b -> IO ()

data MeshJS
type Mesh  = JSRef MeshJS
data Group = Group Mesh
data Scene = Scene (JSRef Scene)



class Object a where mesh :: a -> Mesh

instance Object Mesh  where mesh a = a
instance Object Group where mesh (Group a) = a

data JSVector2
data JSVector3
data JSVector4

foreign import javascript unsafe "new THREE.Vector2($1, $2)"
    buildVector2 :: Double -> Double -> IO (JSRef JSVector2)
foreign import javascript unsafe "new THREE.Vector2($1, $2, $3)"
    buildVector3 :: Double -> Double -> Double -> IO (JSRef JSVector3)
foreign import javascript unsafe "new THREE.Vector4($1, $2, $3, $4)"
    buildVector4 :: Double -> Double -> Double -> Double -> IO (JSRef JSVector4)

foreign import javascript unsafe "$1.x = $2" setX :: JSRef a -> Double -> IO ()
foreign import javascript unsafe "$1.y = $2" setY :: JSRef a -> Double -> IO ()
foreign import javascript unsafe "$1.z = $2" setZ :: JSRef a -> Double -> IO ()
foreign import javascript unsafe "$1.a = $2" setA :: JSRef a -> Double -> IO ()


class Geometry a
class IsMaterial a where material :: a -> JSRef Material


data Material

data Uniform
type Attribute = Uniform
data UniformMap
type AttributeMap = UniformMap

foreign import javascript unsafe "{type: $1, value: $2}"
  buildUniformJS :: JSString -> JSRef a -> IO (JSRef Uniform)

class ToUniform a where
    toUniform :: a -> IO (JSRef Uniform)

instance ToUniform Int               where toUniform a = buildUniformJS (lazyTextToJSString "i" ) (toJSInt a)
instance ToUniform Double            where toUniform a = buildUniformJS (lazyTextToJSString "f" ) (toJSDouble a)
instance ToUniform (JSRef JSVector2) where toUniform a = buildUniformJS (lazyTextToJSString "v2") a
instance ToUniform (JSRef JSVector3) where toUniform a = buildUniformJS (lazyTextToJSString "v2") a
instance ToUniform (JSRef JSVector4) where toUniform a = buildUniformJS (lazyTextToJSString "v4") a


foreign import javascript unsafe "$1.value = $2" setValue :: JSRef Uniform -> JSRef a -> IO()



foreign import javascript unsafe "$r = {}"
  js_empty :: IO (JSRef a)
foreign import javascript unsafe "$1[$2] = $3" js_setProp :: JSRef a -> JSString -> JSRef c -> IO ()


toUniformMap :: [(Text, JSRef Uniform)] -> IO (JSRef UniformMap)
toUniformMap items = do
    list <- js_empty
    mapM_ (\(k, v) -> js_setProp list (lazyTextToJSString k) v) items
    return list

toAttributeMap :: [(Text, JSRef Attribute)] -> IO (JSRef AttributeMap)
toAttributeMap = toUniformMap