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


data Side = FrontSide | BackSide | DoubleSide deriving (Show, Eq, Enum)
data Shading = NoShading | FlatShading | SmoothShading deriving (Show, Eq, Enum)
data Blending = NoBlending | NormalBlending | AdditiveBlending | SubtractiveBlending | MultiplyBlending | CustomBlending deriving (Show, Eq, Enum)

data ShaderMaterial

instance Material ShaderMaterial

foreign import javascript unsafe "new THREE.ShaderMaterial({uniforms: $1, attributes: $2, vertexShader: $3, fragmentShader: $4, transparent: $5, blending: $6, side: $7})"
    buildShaderMaterialJS :: JSRef UniformMap -> JSRef AttributeMap -> JSString -> JSString -> Bool -> Int -> Int  -> IO (JSRef ShaderMaterial)

buildShaderMaterial :: [(Text, JSRef Uniform)] -> [(Text, JSRef Attribute)] -> VertexShader -> FragmentShader -> Bool -> Blending -> Side -> IO (JSRef ShaderMaterial)
buildShaderMaterial u a (VertexShader v) (FragmentShader f) t b s = do
    uniM <- toUniformMap u
    attM <- toAttributeMap a

    buildShaderMaterialJS uniM attM (lazyTextToJSString v) (lazyTextToJSString f) t (fromEnum b) (fromEnum s)



data   VertexShader =   VertexShader Text
data FragmentShader = FragmentShader Text

foreign import javascript unsafe "require('shaders/' + $1)()" js_loadShader :: JSString -> JSString

loadShader :: Text -> Text -> Text
loadShader t n = lazyTextFromJSString . js_loadShader $ lazyTextToJSString (n <> "." <> t)


loadVertexShader   n = VertexShader   $ loadShader "vert" n
loadFragmentShader n = FragmentShader $ loadShader "frag" n

loadShaders n = (loadVertexShader n, loadFragmentShader n)