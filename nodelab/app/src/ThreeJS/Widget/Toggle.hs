{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Toggle where

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
import qualified Object.Widget.Toggle as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import           ThreeJS.Widget.Common


newtype Toggle = Toggle { unToggle :: JSObject.Object }

data Uniforms = Size | ObjectId | Value deriving (Show, Enum)

instance Object Toggle where
    mesh b = (JSObject.getProp "mesh" $ unToggle b) :: IO Mesh

instance Registry.UIWidget Toggle where
    wrapWidget   = Toggle
    unwrapWidget = unToggle

instance Registry.UIWidgetBinding Model.Toggle Toggle


buildToggle :: Model.Toggle -> IO Toggle
buildToggle widget = do
    let size = widget ^. Model.size
    let pos  = widget ^. Model.pos

    group    <- buildGroup
    value    <- toUniform ((if widget ^. Model.value then 1 else 0) :: Int)

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

    JSObject.setProp "label"      label                        (unToggle toggle)
    JSObject.setProp "background" background                   (unToggle toggle)

    return toggle


updateValue :: Model.Toggle -> IO ()
updateValue w =     updateUniformValue Value (toJSInt val) w where
    val = if w ^. Model.value then 1 else 0

instance Clickable Model.Toggle where
    onClick _ toggle = (Just action, toCtxDynamic newToggle) where
        newToggle = toggle & Model.value .~ (not $ toggle ^. Model.value)
        action    = updateValue newToggle

