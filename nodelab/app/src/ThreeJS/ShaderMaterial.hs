{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.ShaderMaterial where

import           Utils.PreludePlus


import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array as JSArray

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import           ThreeJS.Types
import           ThreeJS.Uniform (Uniform(..), UniformMap(..))
import qualified ThreeJS.Uniform as Uniform
import qualified JavaScript.Object as JSObject


data Side     = FrontSide
              | BackSide
              | DoubleSide
              deriving (Show, Eq, Enum)

data Shading  = NoShading
              | FlatShading
              | SmoothShading
              deriving (Show, Eq, Enum)

data Blending = NoBlending
              | NormalBlending
              | AdditiveBlending
              | SubtractiveBlending
              | MultiplyBlending
              | CustomBlending
              deriving (Show, Eq, Enum)

data ShaderMaterial = ShaderMaterial Material

instance IsMaterial ShaderMaterial where material (ShaderMaterial m) = m

foreign import javascript unsafe "new THREE.ShaderMaterial({uniforms: $1, vertexShader: $2, fragmentShader: $3, transparent: $4, blending: $5, side: $6})"
    buildShaderMaterialJS :: UniformMap -> JSString -> JSString -> Bool -> Int -> Int  -> IO Material
foreign import javascript unsafe "$1.uniforms"
    readUniformsJS :: Material -> IO (JSRef ())

buildShaderMaterial :: VertexShader -> FragmentShader -> Bool -> Blending -> Side -> IO ShaderMaterial
buildShaderMaterial (VertexShader v) (FragmentShader f) t b s = do
    uniforms <- Uniform.buildUniformMap
    buildShaderMaterialJS uniforms (lazyTextToJSString v) (lazyTextToJSString f) t (fromEnum b) (fromEnum s) >>= return . ShaderMaterial


setUniforms :: (Enum a, Show a) => ShaderMaterial -> [(a, Uniform)] -> IO ()
setUniforms (ShaderMaterial m) uniforms = do
    u <- readUniformsJS m >>= return . UniformMap . JSObject.fromJSRef
    mapM_ (\(a, v) -> Uniform.setUniform u a v) uniforms

data   VertexShader =   VertexShader Text
data FragmentShader = FragmentShader Text

foreign import javascript unsafe "require('shaders/' + $1)()" js_loadShader :: JSString -> JSString

loadShader :: Text -> Text -> Text
loadShader t n = lazyTextFromJSString . js_loadShader $ lazyTextToJSString (n <> "." <> t)


loadVertexShader   n = VertexShader   $ loadShader "vert" n
loadFragmentShader n = FragmentShader $ loadShader "frag" n

loadShaders n = (loadVertexShader n, loadFragmentShader n)
