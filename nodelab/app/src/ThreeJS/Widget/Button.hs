{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Button where

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
import           ThreeJS.Types
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Text
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector
import           JS.Config as Config
import           ThreeJS.Registry
import qualified Object.Widget.Button as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import           ThreeJS.Widget.Common hiding (buildLabel)


newtype Button = Button { unButton :: JSObject.Object }

data Uniforms = Color | State  deriving (Show, Eq, Enum)

instance Object Button where
    mesh b = (JSObject.getProp "mesh" $ unButton b) :: IO Mesh

instance UIWidget Button where
    wrapWidget   = Button
    unwrapWidget = unButton

instance UIWidgetBinding Model.Button Button

buildLabel text = do
    material <- getTextHUDMaterial
    geom     <- buildTextGeometry text
    mesh     <- buildMesh geom material
    s <- scale mesh
    s `setX` Config.fontSize
    s `setY` Config.fontSize
    p <- position mesh
    p `setY` (4.0 + 10.0)
    let width = Config.fontSize * (calculateTextWidth text)
    return (mesh, width)


buttonPadding = 20
buttonWidth text = Config.fontSize * (calculateTextWidth text) + 2 * buttonPadding


buildButton :: Model.Button -> IO Button
buildButton (Model.Button bid label state pos size) = do
    group    <- buildGroup
    state    <- toUniform (0 :: Int)

    background <- do
        let (vs, fs) = loadShaders "button"
        color    <- buildVector4 1.0 0.0 1.0 1.0 >>= toUniform
        sizeU    <- toUniform size
        objectId <- buildVector3 ((fromIntegral $ bid `mod` 256) / 255.0) ((fromIntegral $ bid `div` 256) / 255.0) 0.0 >>= toUniform

        geom     <- buildNormalizedPlaneGeometry

        material <- buildShaderMaterial vs fs True NormalBlending DoubleSide
        setUniforms material [ (Color    , color    )
                             , (State    , state    )
                             ]
        setUniforms material [ (Size     , sizeU    ) ]
        mesh     <- buildMesh geom material

        scaleBy size mesh

        return mesh

    label <- do
        (l, width) <- buildLabel label
        p          <- position l
        p       `setY` (4.0 + size ^. y / 2.0)
        p       `setX` ((size ^. x - width) / 2.0)
        return l

    group `add` background
    group `add` label

    mesh   <- mesh group
    moveTo pos mesh


    (button, uniforms) <- buildSkeleton mesh
    Uniform.setUniform uniforms State state

    JSObject.setProp "background" background (unButton button)
    JSObject.setProp "label"      label      (unButton button)

    return button

updateState :: Model.Button -> IO ()
updateState b = do
    updateUniformValue State (toJSInt $ fromEnum $ b ^. Model.state) b

instance HandlesMouseOver Model.Button where
    onMouseOver b = (Just action, toCtxDynamic neModelutton) where
        action    = updateState neModelutton
        neModelutton = b & Model.state .~ Model.Focused

instance HandlesMouseOut Model.Button where
    onMouseOut  b = (Just action, toCtxDynamic neModelutton) where
        action    = updateState neModelutton
        neModelutton = b & Model.state .~ Model.Normal

instance Clickable Model.Button where
    onClick pos b = (Just action, toCtxDynamic neModelutton) where
        action    = updateState neModelutton
        neModelutton = b & Model.state .~ Model.Pressed

instance DblClickable Model.Button where
    onDblClick pos b = (Just action, toCtxDynamic neModelutton) where
        action    = updateState neModelutton
        neModelutton = b & Model.state .~ Model.Disabled
