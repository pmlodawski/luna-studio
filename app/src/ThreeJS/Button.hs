{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Button where

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


type Box    = Mesh
type Label  = Mesh
-- newtype Object = Object (JSRef ()) deriving (Typeable)
data Button = Button Group Box Label Attribute

instance Object Button where
    mesh (Button m _ _ _) = mesh m

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


buildButton :: Text -> Vector2 Double -> Vector2 Double -> IO Button
buildButton label pos size = do
    group    <- buildGroup
    state    <- toAttribute (0 :: Int)

    background <- do
        let (vs, fs) = loadShaders "button"
        attributes <- buildAttributeMap
        uniforms <- buildAttributeMap
        color    <- buildVector4 1.0 0.0 1.0 1.0 >>= toAttribute
        sizeU    <- buildVector2 (size ^. x) (size ^. y) >>= toAttribute
        geom     <- buildPlaneGeometry 1.0 1.0
        setAttribute uniforms "color" color
        setAttribute uniforms "size" sizeU
        setAttribute uniforms "state" state
        Geometry.translate geom 0.5 0.5 0.0
        material <- buildShaderMaterial uniforms attributes vs fs True NormalBlending DoubleSide
        mesh     <- buildMesh geom material
        s        <- scale mesh
        s     `setX` (size ^. x)
        s     `setY` (size ^. y)
        return mesh

    label <- do
        (l, width) <- buildLabel label
        p          <- position l
        p       `setY` (4.0 + size ^. y / 2.0)
        p       `setX` ((size ^. x - width) / 2.0)
        return l

    group `add` background
    group `add` label

    p <- position (mesh group)
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    return $ Button group background label state