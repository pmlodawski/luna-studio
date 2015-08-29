{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Slider where

import           Utils.PreludePlus


import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import qualified JavaScript.Object as JSObject

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Event.Mouse    as Mouse
import           Event.Keyboard (KeyMods(..))
import           ThreeJS.Types
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Converters
import           ThreeJS.Text
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector
import           ThreeJS.Registry as Registry
import qualified Object.Widget.Slider as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import           ThreeJS.Widget.Common
import           Object.UITypes


newtype Slider = Slider { unSlider :: JSObject.Object }


data Uniforms = Value | Focus deriving (Show)
instance Uniform.UniformKey Uniforms

data Components = Background | Label | ValueLabel deriving (Show)
instance Registry.ComponentKey Components

instance Object Slider where
    mesh b = (JSObject.getProp "mesh" $ unSlider b) :: IO Mesh

instance Registry.UIWidget Slider where
    wrapWidget = Slider
    unwrapWidget = unSlider


buildValueLabel :: (Model.IsSlider a) => Model.Slider a -> IO Mesh
buildValueLabel s = do
    (mesh, width) <- buildLabel 0.8 AlignRight (Text.pack $ Model.displayValue s)
    moveBy (Vector2 (s ^. Model.size . x - 5.0) (5.0 + s ^. Model.size . y / 2.0)) mesh
    return mesh

instance (Model.IsSlider a) => Registry.UIWidgetBinding (Model.Slider a) Slider where
    build widget = do
        let pos  = widget ^. Model.pos
        let size = widget ^. Model.size

        group     <- buildGroup
        sliderPos <- toUniform $ widget ^. Model.normValue
        focus     <- toUniform (0 :: Int)

        label <- do
            (mesh, width) <- buildLabel 1.0 AlignLeft (widget ^. Model.label)
            moveBy (Vector2 4.0 (5.0 + size ^. y / 2.0)) mesh
            return mesh

        valueLabel <- buildValueLabel widget

        background <- buildBackground "slider" widget [ (Value    , sliderPos )
                                                      , (Focus    , focus     )
                                                      ]
        group `add` background
        group `add` label
        group `add` valueLabel

        mesh   <- mesh group
        moveTo pos mesh

        (slider, uniforms) <- buildSkeleton mesh
        Uniform.setUniform uniforms Value sliderPos
        Uniform.setUniform uniforms Focus focus

        addComponents slider [ (Label, label)
                             , (Background, background)
                             , (ValueLabel, valueLabel)
                             ]

        return slider

setValueLabel :: (Model.IsSlider a) => Model.Slider a -> IO ()
setValueLabel widget = do
    ref        <- Registry.lookup widget
    group      <- Registry.readContainer ref
    valueLabel <- Registry.readComponent ValueLabel ref :: IO Mesh
    group `remove` valueLabel

    valueLabel' <- buildValueLabel widget
    group `add` valueLabel'
    addComponent ref ValueLabel valueLabel'

updateValue :: (Model.IsSlider a) => Model.Slider a -> IO ()
updateValue widget = do
    updateUniformValue Value (toJSDouble $ widget ^. Model.normValue) widget
    setValueLabel widget


keyModMult :: KeyMods -> Double
keyModMult mods = case mods of
    KeyMods True  True  _ _ -> 1000.0
    KeyMods False True  _ _ ->  100.0
    KeyMods True  False _ _ ->   10.0
    otherwise               ->    1.0

instance (Model.IsSlider a) => Draggable (Model.Slider a) where
    mayDrag LeftButton _ _       = True
    mayDrag _          _ _       = False
    onDragStart state slider     = (Just action, toCtxDynamic slider) where
                          action = setCursor "pointer"
    onDragMove  state slider     = (Just action, toCtxDynamic newSlider) where
                    delta        = if (abs $ diff ^. x) > (abs $ diff ^. y) then -diff ^. x /  divider
                                                                            else  diff ^. y / (divider * 10.0)
                    width        = slider ^. Model.size . x
                    divider      = width * (keyModMult $ state ^. keyMods)
                    diff         = state ^. currentPos - state ^. previousPos
                    newNormValue = (slider ^. Model.normValue) - delta
                    newSlider    = Model.setNormValue newNormValue slider
                    action       = do
                        setCursor "-webkit-grabbing"
                        updateValue newSlider
    onDragEnd  state slider = (Just $ action, newSlider) where
        action = do
            fromMaybe (return ()) otherAction
            setCursor "default"
        (otherAction, newSlider) = onDragMove state slider

instance  (Model.IsSlider a) => DblClickable   (Model.Slider a) where
    onDblClick pos slider     = (Just action, toCtxDynamic newSlider) where
                normValue     = (pos ^. x) / (slider ^. Model.size . x)
                newSlider     = Model.setNormValue normValue slider
                action        = do
                    updateValue newSlider

instance  (Model.IsSlider a) => HandlesMouseOver (Model.Slider a) where
    onMouseOver b = (Just action, toCtxDynamic b) where
        action    = updateUniformValue Focus (toJSInt 1) b

instance  (Model.IsSlider a) => HandlesMouseOut (Model.Slider a) where
    onMouseOut  b = (Just action, toCtxDynamic b) where
        action    = updateUniformValue Focus (toJSInt 0) b
