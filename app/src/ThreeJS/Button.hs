{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Button where

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
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Text
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector


type Box    = Mesh
type Label  = Mesh
data Button = Button Group Box Label (JSRef Uniform)

instance Object Button where
    mesh (Button m _ _ _) = mesh m

buildLabel text = do
    material <- getTextMaterial
    geom     <- buildTextGeometry text
    mesh     <- buildMesh geom material
    s <- scale mesh
    s `setX` 0.3
    s `setY` 0.3
    p <- position mesh
    p `setY` (4.0 + 10.0)
    return mesh

buildButton :: Text -> Vector2 Double -> Vector2 Double -> IO Button
buildButton label pos size = do
    group  <- buildGroup
    state    <- toUniform (0 :: Int)

    background <- do
        let (vs, fs) = loadShaders "button"
        color    <- buildVector4 1.0 0.0 1.0 1.0 >>= toUniform
        sizeU     <- buildVector2 (size ^. x) (size ^. y) >>= toUniform
        geom     <- buildPlaneGeometry 1.0 1.0
        Geometry.translate geom 0.5 0.5 0.0
        material <- buildShaderMaterial [("color", color), ("size", sizeU), ("state", state)] [] vs fs True NormalBlending DoubleSide
        mesh     <- buildMesh geom material
        s        <- scale mesh
        s `setX` (size ^. x)
        s `setY` (size ^. y)
        return mesh

    label <- buildLabel label

    group `add` background
    group `add` label

    p <- position (mesh group)
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    return $ Button group background label state