{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module ThreeJS.Uniform where

import           Utils.PreludePlus
import           Utils.Vector
import qualified GHCJS.Prim.Internal.Build as Build

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import           Data.JSString ( JSString )
import qualified Data.JSString as JSString

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array as JSArray

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified JavaScript.Object as JSObject

import           GHCJS.Prim
import           ThreeJS.Converters
import           ThreeJS.Types
import qualified Data.Char as Char


newtype UniformMap = UniformMap { unUniformMap :: JSObject.Object }
newtype Uniform    = Uniform    { unUniform    :: JSObject.Object }

buildUniformMap :: IO UniformMap
buildUniformMap = do
    m <- JSObject.create
    copyCommonUniforms $ JSObject.getJSRef m
    return $ UniformMap m

foreign import javascript unsafe "for(var k in $$.commonUniforms) {$1[k] = $$.commonUniforms[k]; };" copyCommonUniforms :: JSRef a -> IO ()


locaseFirst :: String -> String
locaseFirst (head:tail) = Char.toLower head : tail
locaseFirst [] = []


setUniform :: (Enum a, Show a) => UniformMap -> a -> Uniform -> IO ()
setUniform m a v = JSObject.setProp (JSString.pack $ locaseFirst $ show a) (JSObject.getJSRef $ unUniform v) (unUniformMap m)


buildUniform :: Text -> JSRef a -> IO Uniform
buildUniform t v = do
    o <- JSObject.create
    let x = JSString.getJSRef $ lazyTextToJSString t
    JSObject.setProp (JSString.pack "type") x o
    JSObject.setProp (JSString.pack "value") v o
    return $ Uniform o

setValue :: Uniform -> JSRef a -> IO ()
setValue o v = JSObject.setProp (JSString.pack "value") v (unUniform o)

class ToUniform a b | a -> b where
    toUniform      :: a -> IO Uniform

instance ToUniform Int Int where
    toUniform      a = buildUniform "i"  (toJSInt a)
instance ToUniform Double Double where
    toUniform      a = buildUniform "f"  (toJSDouble a)
instance ToUniform (JSRef JSVector2) JSVector2 where
    toUniform      a = buildUniform "v2" a
instance ToUniform (Vector2 Double) JSVector2 where
    toUniform      a = do
        vec <- buildVector2 (a ^. x) (a ^.y)
        buildUniform "v2" vec

instance ToUniform (JSRef JSVector3) JSVector3 where
    toUniform      a = buildUniform "v3" a
instance ToUniform (JSRef JSVector4) JSVector4 where
    toUniform      a = buildUniform "v4" a
