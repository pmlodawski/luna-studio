{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Slider where

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
import qualified Event.Mouse as Mouse
import           ThreeJS.Types
import           ThreeJS.Mesh
import           ThreeJS.PlaneGeometry
import           ThreeJS.ShaderMaterial
import           ThreeJS.Converters
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector
import           ThreeJS.Registry
import qualified Object.Widget.Slider as WB
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic


newtype Slider = Slider { unSlider :: JSObject.Object }

instance Object Slider where
    mesh b = (JSObject.getProp "mesh" $ unSlider b) :: IO Mesh

buildSlider :: WB.Slider -> IO Slider
buildSlider s@(WB.Slider bid pos size minValue maxValue value) = do
    group    <- buildGroup
    sliderPos <- toAttribute $ WB.sliderPosition s

    background <- do
        let (vs, fs) = loadShaders "slider"
        attributes <- buildAttributeMap
        uniforms   <- buildAttributeMap
        sizeU     <- buildVector2 (size ^. x) (size ^. y) >>= toAttribute
        objectId  <- buildVector3 ((fromIntegral $ bid `mod` 256) / 255.0) ((fromIntegral $ bid `div` 256) / 255.0) 0.0 >>= toAttribute
        geom      <- buildPlaneGeometry 1.0 1.0
        setAttribute uniforms "size" sizeU
        setAttribute uniforms "objectId" objectId
        setAttribute uniforms "value" sliderPos
        Geometry.translate geom 0.5 0.5 0.0
        material <- buildShaderMaterial uniforms attributes vs fs True NormalBlending DoubleSide
        mesh     <- buildMesh geom material
        s        <- scale mesh
        s     `setX` (size ^. x)
        s     `setY` (size ^. y)
        return mesh

    group `add` background

    p <- (mesh group) >>= position
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    uniforms <- JSObject.create
    JSObject.setProp "value"    (JSObject.getJSRef $ unAttribute sliderPos) uniforms

    slider <- JSObject.create

    JSObject.setProp "mesh" (unGroup group)  slider
    JSObject.setProp "background" background slider
    JSObject.setProp "uniforms" (JSObject.getJSRef uniforms) slider

    return $ Slider slider

getFromRegistry :: WB.Slider -> IO Slider
getFromRegistry b = (getFromRegistryJS sliderId >>= return . Slider . JSObject.fromJSRef)
    where sliderId = b ^. WB.refId

putToRegistry :: WB.Slider -> Slider -> IO ()
putToRegistry b u = putToRegistryJS sliderId (JSObject.getJSRef $ unSlider u)
    where sliderId = b ^. WB.refId

removeFromRegistry :: WB.Slider -> IO ()
removeFromRegistry b = removeFromRegistryJS sliderId
    where sliderId = b ^. WB.refId

setUniform :: Text -> JSRef a -> WB.Slider -> IO ()
setUniform n v w = do
    bref     <- getFromRegistry w
    uniforms <- JSObject.getProp "uniforms" (unSlider bref) >>= return . JSObject.fromJSRef
    uniform  <- JSObject.getProp (lazyTextToJSString n) uniforms >>= return . Attribute . JSObject.fromJSRef
    setValue uniform v

updateValue :: WB.Slider -> IO ()
updateValue s = setUniform "value" (toJSDouble $ WB.sliderPosition s) s

instance Clickable WB.Slider where
    onClick pos b = (Just action, toCtxDynamic newSlider) where
        action    = do
            updateValue newSlider
            putStrLn $ show value

        newSlider = b & WB.value .~ value
        normValue = (fromIntegral $ pos ^. x) / (b ^. WB.size . x)
        value     = b ^. WB.minValue + normValue * (b ^. WB.maxValue - b ^. WB.minValue )

instance HandlesMouseMove WB.Slider where
    onMouseMove Mouse.LeftButton pos = onClick pos
    onMouseMove _ _ = noUpdate

instance Draggable        WB.Slider where
    mayDrag         Mouse.LeftButton _ _ = True
    onDragEnd       _ _        b =  (Just $ putStrLn "Dragend", toCtxDynamic b) where
    onDragMove      _ pos      b =  (Just action, toCtxDynamic newSlider) where
        action       = do
            updateValue newSlider
            putStrLn $ "I am dragging!"
            putStrLn $ show value

        newSlider    = b & WB.value .~ value
        normValue    = (fromIntegral $ pos ^. x) / (b ^. WB.size . x)
        boundedValue = max 1.0 $ min 1.0 normValue
        value        = b ^. WB.minValue + normValue * (b ^. WB.maxValue - b ^. WB.minValue )



-- instance Clickable WB.Button where
--     onClick pos b = (Just action, toCtxDynamic newButton) where
--         action    = updateState newButton
--         newButton = b & WB.state .~ WB.Pressed
--
-- instance DblClickable WB.Button where
--     onDblClick pos b = (Just action, toCtxDynamic newButton) where
--         action    = updateState newButton
--         newButton = b & WB.state .~ WB.Disabled