{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Number where

import           Utils.PreludePlus


import           GHCJS.Foreign hiding (Number)
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
import           ThreeJS.Registry as Registry
import qualified Object.Widget.Number as WB
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import qualified ThreeJS.Widget.Slider  as Slider
import           ThreeJS.Widget.Common


newtype Number = Number { unNumber :: JSObject.Object }

data Uniforms = Size | ObjectId | Value | Focus deriving (Show, Enum, Eq)

instance Object Number where
    mesh b = (JSObject.getProp "mesh" $ unNumber b) :: IO Mesh

instance Registry.UIWidget Number where
    wrapWidget = Number
    unwrapWidget = unNumber

instance Registry.UIWidgetBinding (WB.Number a) Number

buildValueLabel w = do
    let sliderWidth = w ^. WB.size ^. x
    let text =  Text.pack $ show $ w ^. WB.value
    material <- getTextHUDMaterial
    geom     <- buildTextGeometry text
    mesh     <- buildMesh geom material
    s <- scale mesh
    s `setX` (Config.fontSize * 0.8)
    s `setY` (Config.fontSize * 0.8)

    let width = Config.fontSize * 0.8 * (calculateTextWidth text)
    p <- position mesh
    p `setY` (5.0 + w ^. WB.size ^. y / 2.0)
    p `setX` (sliderWidth - width - 5.0)
    p `setZ` 0.001
    return mesh

buildNumber :: (Show a) => WB.Number a -> IO Number
buildNumber s = do
    let bid  = objectId s
    let pos  = s ^. WB.pos
    let size = s ^. WB.size

    group     <- buildGroup
    focus     <- toUniform (0 :: Int)

    label <- do
        (mesh, width) <-  buildLabel 1.0 AlignLeft (s ^. WB.label)
        position      <-  position mesh
        position   `setY` (5.0 + size ^. y / 2.0)
        position   `setX` 4.0
        position   `setZ` 0.001
        return mesh

    background <- do
        let (vs, fs) = loadShaders "slider"
        uniforms  <- Uniform.buildUniformMap
        sliderPos <- toUniform (0 :: Double)
        sizeU     <- toUniform size
        objectId  <- buildVector3 ((fromIntegral $ bid `mod` 256) / 255.0) ((fromIntegral $ bid `div` 256) / 255.0) 0.0 >>= toUniform

        geom      <- buildNormalizedPlaneGeometry

        material <- buildShaderMaterial vs fs True NormalBlending DoubleSide
        setUniforms material [ (Size     , sizeU     )
                             , (ObjectId , objectId  )
                             , (Value    , sliderPos )
                             , (Focus    , focus     )
                             ]
        mesh     <- buildMesh geom material
        scaleBy size mesh

        return mesh

    valueLabel <- buildValueLabel s

    group `add` background
    group `add` label
    group `add` valueLabel

    p <- (mesh group) >>= position
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    uniforms <- JSObject.create
    JSObject.setProp "focus"    (JSObject.getJSRef $ unUniform focus) uniforms

    number <- JSObject.create

    JSObject.setProp "mesh"       (unGroup group)              number
    JSObject.setProp "label"      label                        number
    JSObject.setProp "valueLabel" valueLabel                   number
    JSObject.setProp "background" background                   number
    JSObject.setProp "uniforms"   (JSObject.getJSRef uniforms) number

    return $ Number number
