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
import qualified Object.Widget.Number as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import qualified ThreeJS.Widget.Slider  as Slider
import           ThreeJS.Widget.Common


newtype Number = Number { unNumber :: JSObject.Object }

data Uniforms = Size | ObjectId | Value | Focus deriving (Show)
instance Uniform.UniformKey Uniforms

data Components = Background | Label | ValueLabel deriving (Show)
instance Registry.ComponentKey Components

instance Object Number where
    mesh b = (JSObject.getProp "mesh" $ unNumber b) :: IO Mesh

instance Registry.UIWidget Number where
    wrapWidget = Number
    unwrapWidget = unNumber

instance Registry.UIWidgetBinding (Model.Number a) Number

buildValueLabel :: (Show a) => Model.Number a -> IO Mesh
buildValueLabel s = do
    (mesh, width) <- buildLabel 0.8 AlignRight (Text.pack $ show $ s ^. Model.value)
    moveBy (Vector2 (s ^. Model.size . x - 4.0) (5.0 + s ^. Model.size . y / 2.0)) mesh
    return mesh

buildNumber :: (Show a) => Model.Number a -> IO Number
buildNumber widget = do
    let pos  = widget ^. Model.pos
    let size = widget ^. Model.size

    group     <- buildGroup
    focus     <- toUniform (0 :: Int)

    label <- do
        (mesh, width) <- buildLabel 1.0 AlignLeft (widget ^. Model.label)
        moveBy (Vector2 4.0 (5.0 + size ^. y / 2.0)) mesh
        return mesh

    sliderPos  <- toUniform (0.0 :: Double)
    background <- buildBackground "slider" widget [ (Value, sliderPos)
                                                  , (Focus, focus    )
                                                  ]

    valueLabel <- buildValueLabel widget

    group `add` background
    group `add` label
    group `add` valueLabel

    mesh   <- mesh group
    moveTo pos mesh

    (number, uniforms) <- buildSkeleton mesh
    Uniform.setUniform uniforms Focus focus

    addComponents number [ (Label, label)
                         , (Background, background)
                         , (ValueLabel, valueLabel)
                         ]

    return number
