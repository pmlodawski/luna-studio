{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Slider where

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
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform


newtype Slider = Slider { unSlider :: JSObject.Object }

instance Object Slider where
    mesh b = (JSObject.getProp "mesh" $ unSlider b) :: IO Mesh

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

buildValueLabel w = do
    let sliderWidth = w ^. WB.size ^. x
    let text =  Text.pack $ WB.displayValue w
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

buildSlider :: (WB.IsSlider a) => WB.Slider a -> IO Slider
buildSlider s = do
    let bid  = objectId s
    let pos  = s ^. WB.pos
    let size = s ^. WB.size

    group     <- buildGroup
    sliderPos <- toUniform $ s ^. WB.normValue
    focus     <- toUniform (0 :: Int)

    label <- do
        (mesh, width) <-  buildLabel (s ^. WB.label)
        position      <-  position mesh
        position   `setY` (5.0 + size ^. y / 2.0)
        position   `setX` 4.0
        position   `setZ` 0.001
        return mesh

    background <- do
        let (vs, fs) = loadShaders "slider"
        uniforms  <- Uniform.buildUniformMap
        sizeU     <- buildVector2 (size ^. x) (size ^. y) >>= toUniform
        objectId  <- buildVector3 ((fromIntegral $ bid `mod` 256) / 255.0) ((fromIntegral $ bid `div` 256) / 255.0) 0.0 >>= toUniform
        geom      <- buildPlaneGeometry 1.0 1.0
        Uniform.setUniform uniforms "size" sizeU
        Uniform.setUniform uniforms "objectId" objectId
        Uniform.setUniform uniforms "value" sliderPos
        Uniform.setUniform uniforms "focus" focus
        Geometry.translate geom 0.5 0.5 0.0
        material <- buildShaderMaterial uniforms vs fs True NormalBlending DoubleSide
        mesh     <- buildMesh geom material
        s        <- scale mesh
        s     `setX` (size ^. x)
        s     `setY` (size ^. y)
        return mesh

    valueLabel <- buildValueLabel s

    group `add` background
    group `add` label
    group `add` valueLabel

    p <- (mesh group) >>= position
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    uniforms <- JSObject.create
    JSObject.setProp "value"    (JSObject.getJSRef $ unUniform sliderPos) uniforms
    JSObject.setProp "focus"    (JSObject.getJSRef $ unUniform focus    ) uniforms

    slider <- JSObject.create

    JSObject.setProp "mesh"       (unGroup group)              slider
    JSObject.setProp "label"      label                        slider
    JSObject.setProp "valueLabel" valueLabel                   slider
    JSObject.setProp "background" background                   slider
    JSObject.setProp "uniforms"   (JSObject.getJSRef uniforms) slider

    return $ Slider slider

getFromRegistry :: WB.Slider a -> IO Slider
getFromRegistry b = (getFromRegistryJS sliderId >>= return . Slider . JSObject.fromJSRef)
    where sliderId = b ^. WB.refId

putToRegistry :: WB.Slider a -> Slider -> IO ()
putToRegistry b u = putToRegistryJS sliderId (JSObject.getJSRef $ unSlider u)
    where sliderId = b ^. WB.refId

removeFromRegistry :: WB.Slider a -> IO ()
removeFromRegistry b = removeFromRegistryJS sliderId
    where sliderId = b ^. WB.refId

setUniform :: Text -> JSRef a -> WB.Slider a -> IO ()
setUniform n v w = do
    bref     <- getFromRegistry w
    uniforms <- JSObject.getProp "uniforms"             (unSlider bref) >>= return .           JSObject.fromJSRef
    uniform  <- JSObject.getProp (lazyTextToJSString n)  uniforms       >>= return . Uniform . JSObject.fromJSRef
    Uniform.setValue uniform v

setValueLabel :: (WB.IsSlider a) => WB.Slider a -> IO ()
setValueLabel w = do
    ref        <- getFromRegistry w
    group      <- JSObject.getProp "mesh"        (unSlider ref) >>= return . Group
    valueLabel <- JSObject.getProp "valueLabel"  (unSlider ref) :: IO (JSRef MeshJS)
    group `remove` valueLabel

    valueLabel' <- buildValueLabel w

    group `add` valueLabel'
    JSObject.setProp "valueLabel" valueLabel'    (unSlider ref)



updateValue :: (WB.IsSlider a) => WB.Slider a -> IO ()
updateValue s = do
    setUniform "value" (toJSDouble $ s ^. WB.normValue) s
    setValueLabel s


keyModMult :: KeyMods -> Double
keyModMult mods = case mods of
    KeyMods True  True  _ _ -> 1000.0
    KeyMods False True  _ _ ->  100.0
    KeyMods True  False _ _ ->   10.0
    otherwise               ->    1.0

instance (WB.IsSlider a) => Draggable (WB.Slider a) where
    mayDrag Mouse.LeftButton _ _ = True
    mayDrag _                _ _ = False
    onDragStart state slider     = (Just action, toCtxDynamic slider) where
                          action = setCursor "pointer"
    onDragMove  state slider     = (Just action, toCtxDynamic newSlider) where
                    delta        = if (abs $ diff ^. x) > (abs $ diff ^. y) then -diff ^. x / divider
                                                                            else  diff ^. y / (divider * 10.0)
                    width        = slider ^. WB.size . x
                    divider      = width * (keyModMult $ state ^. keyMods)
                    diff         = state ^. currentPos - state ^. previousPos
                    newNormValue = (slider ^. WB.normValue) - delta
                    newSlider    = WB.setNormValue newNormValue slider
                    action       = do
                        setCursor "-webkit-grabbing"
                        updateValue newSlider
    onDragEnd  state slider = (Just $ action, newSlider) where
        action = do
            fromMaybe (return ()) otherAction
            setCursor "default"
        (otherAction, newSlider) = onDragMove state slider

instance  (WB.IsSlider a) => DblClickable   (WB.Slider a) where
    onDblClick pos slider     = (Just action, toCtxDynamic newSlider) where
                normValue     = (pos ^. x) / (slider ^. WB.size . x)
                newSlider     = WB.setNormValue normValue slider
                action        = do
                    updateValue newSlider

instance  (WB.IsSlider a) => HandlesMouseOver (WB.Slider a) where
    onMouseOver b = (Just action, toCtxDynamic b) where
        action    = do
            setUniform "focus" (toJSInt 1) b

instance  (WB.IsSlider a) => HandlesMouseOut (WB.Slider a) where
    onMouseOut  b = (Just action, toCtxDynamic b) where
        action    = do
            setUniform "focus" (toJSInt 0) b
