{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Common where

import           Utils.PreludePlus


import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array  as JSArray
import qualified JavaScript.Object as JSObject

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Event.Mouse    as Mouse
import qualified Event.Keyboard as Keyboard
import           Event.Keyboard (KeyMods(..))
import           ThreeJS.Types
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Converters
import           ThreeJS.Text
import qualified ThreeJS.Geometry as Geometry
import           JS.Config as Config
import           Utils.Vector
import           ThreeJS.Registry
import qualified Object.Widget.Slider as WB
import           Object.Widget
import           Object.UITypes
import           GHCJS.Prim
import           Utils.CtxDynamic
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform

data TextAlignment = AlignLeft | AlignCenter | AlignRight deriving (Show, Eq)

buildLabel :: Double -> TextAlignment -> Text -> IO (Mesh, Double)
buildLabel fontSize align text = do
    let width = Config.fontSize * fontSize * (calculateTextWidth text)
    material <- getTextHUDMaterial
    geom     <- buildTextGeometry text
    mesh     <- buildMesh geom material
    scaleBy (pure $ Config.fontSize * fontSize) mesh
    let offsetX = (case align of
                    AlignLeft   ->  0.0
                    AlignCenter -> -width / 2.0
                    AlignRight  -> -width)
    moveBy (Vector2 offsetX 0.0) mesh

    return (mesh, width)

objectIdToUniform :: WidgetId -> IO Uniform
objectIdToUniform oid = buildVector3 ((fromIntegral $ oid `mod` 256) / 255.0) ((fromIntegral $ oid `div` 256) / 255.0) 0.0 >>= toUniform
-- TODO: support > 2^16 ids

data GenericUniforms = Size | ObjectId deriving (Show)
instance Uniform.UniformKey GenericUniforms

buildBackground :: (Uniform.UniformKey a, IsDisplayObject b) => Text -> WidgetId -> b -> [(a, Uniform)] -> IO Mesh
buildBackground shader oid model uniforms = do
    let (vs, fs) = loadShaders shader
    sizeU     <- toUniform $ objectSize model
    objectId  <- objectIdToUniform $ oid

    geom      <- buildNormalizedPlaneGeometry
    material  <- buildShaderMaterial vs fs True NormalBlending DoubleSide

    setUniforms material [ (Size     , sizeU     )
                         , (ObjectId , objectId  )
                         ]
    setUniforms material uniforms

    mesh      <- buildMesh geom material
    scaleBy (objectSize $ model) mesh
    pos       <- position mesh
    pos `setZ` (-0.000002)
    return mesh
