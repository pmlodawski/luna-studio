{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Toggle where

import           Utils.PreludePlus


import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Object as JSObject

import           ThreeJS.Types
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Converters
import           ThreeJS.Text
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector
import           ThreeJS.Registry as Registry
import qualified Object.Widget.Toggle as Model
import           Object.Widget
import           Utils.CtxDynamic
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import           ThreeJS.Widget.Common
import           Foreign.Marshal.Utils (fromBool)
import           GHCJS.Prim


newtype Toggle = Toggle { unToggle :: JSObject.Object }

data Uniforms = Size | ObjectId | Value deriving (Show)
instance Uniform.UniformKey Uniforms

data Components = Background | Label deriving (Show)
instance Registry.ComponentKey Components

instance Object Toggle where
    mesh b = (JSObject.getProp "mesh" $ unToggle b) :: IO Mesh

instance Registry.UIWidget Toggle where
    wrapWidget   = Toggle
    unwrapWidget = unToggle

instance Registry.UIWidgetBinding Model.Toggle Toggle


intValue :: Model.Toggle -> Int
intValue widget = fromBool $ widget ^. Model.value

buildToggle :: Model.Toggle -> IO Toggle
buildToggle widget = do
    let size = widget ^. Model.size
    let pos  = widget ^. Model.pos

    group    <- buildGroup
    value    <- toUniform $ intValue widget

    label <- do
        (mesh, width) <- buildLabel 1.0 AlignLeft (widget ^. Model.label)
        moveBy (Vector2 4.0 (5.0 + size ^. y / 2.0)) mesh
        return mesh

    background <- buildBackground "toggle" widget [(Value, value)]

    group `add` background
    group `add` label

    mesh   <- mesh group
    moveTo pos mesh

    (toggle, uniforms) <- buildSkeleton mesh
    Uniform.setUniform uniforms Value value

    addComponents toggle [ (Label     , label      )
                         , (Background, background )
                         ]
    return toggle


updateValue :: Model.Toggle -> IO ()
updateValue widget = updateUniformValue Value (toJSInt $ intValue widget) widget

instance Clickable Model.Toggle where
    onClick _ toggle = (Just action, toCtxDynamic newToggle) where
        newToggle = toggle & Model.value .~ (not $ toggle ^. Model.value)
        action    = updateValue newToggle

