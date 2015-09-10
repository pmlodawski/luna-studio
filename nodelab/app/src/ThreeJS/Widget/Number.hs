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
import           Object.UITypes
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


buildValueLabel :: (Show a) => Model.Number a -> IO Mesh
buildValueLabel s = do
    (mesh, width) <- buildLabel 0.8 AlignRight (Text.pack $ show $ s ^. Model.value)
    moveBy (Vector2 (s ^. Model.size . x - 4.0) (5.0 + s ^. Model.size . y / 2.0)) mesh
    return mesh

instance (Show a) => Registry.UIWidgetBinding (Model.Number a) Number where
    build oid model = do
        let pos  = model ^. Model.pos
        let size = model ^. Model.size

        group     <- buildGroup
        focus     <- toUniform (0 :: Int)

        label <- do
            (mesh, width) <- buildLabel 1.0 AlignLeft (model ^. Model.label)
            moveBy (Vector2 4.0 (5.0 + size ^. y / 2.0)) mesh
            return mesh

        sliderPos  <- toUniform (0.0 :: Double)
        background <- buildBackground "slider" oid model [ (Value, sliderPos)
                                                         , (Focus, focus    )
                                                         ]

        valueLabel <- buildValueLabel model

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

setValueLabel :: (Show a) => WidgetId -> Model.Number a -> IO ()
setValueLabel oid widget = do
    ref        <- Registry.lookup oid :: IO Number
    group      <- Registry.readContainer ref
    valueLabel <- Registry.readComponent ValueLabel ref :: IO Mesh
    group `remove` valueLabel

    valueLabel' <- buildValueLabel widget
    group `add` valueLabel'
    addComponent ref ValueLabel valueLabel'


instance Focusable (Model.Number a) where
    mayFocus _ _ _ _ = True

bumpValue :: Int -> WidgetFile s DisplayObject -> Model.Number Int -> WidgetUpdate
bumpValue amount file widget = (Just action, toCtxDynamic newWidget) where
                currVal      = widget ^. Model.value
                newWidget    = widget &  Model.value .~ (currVal + amount)
                action       = setValueLabel (file ^. objectId) newWidget

instance HandlesKeyUp (Model.Number Int) where
    onKeyUp 'Q' = bumpValue  100000
    onKeyUp 'W' = bumpValue   10000
    onKeyUp 'E' = bumpValue    1000
    onKeyUp 'R' = bumpValue     100
    onKeyUp 'T' = bumpValue      10
    onKeyUp 'Y' = bumpValue       1

    onKeyUp 'A' = bumpValue (-100000)
    onKeyUp 'S' = bumpValue ( -10000)
    onKeyUp 'D' = bumpValue (  -1000)
    onKeyUp 'F' = bumpValue (   -100)
    onKeyUp 'G' = bumpValue (    -10)
    onKeyUp 'H' = bumpValue (     -1)

    onKeyUp _   = noUpdate
