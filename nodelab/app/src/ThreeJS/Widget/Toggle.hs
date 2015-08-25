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
import qualified Object.Widget.Toggle as WB
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform


newtype Toggle = Toggle { unToggle :: JSObject.Object }

data Uniforms = Size | ObjectId | Value deriving (Show, Enum)

instance Object Toggle where
    mesh b = (JSObject.getProp "mesh" $ unToggle b) :: IO Mesh

instance Registry.UIWidget Toggle where
    wrapWidget   = Toggle
    unwrapWidget = unToggle

instance Registry.UIWidgetBinding WB.Toggle Toggle

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

buildToggle :: WB.Toggle -> IO Toggle
buildToggle w = do
    let size = w ^. WB.size
    let oid  = w ^. WB.refId
    let pos  = w ^. WB.pos
    group    <- buildGroup
    value    <- toUniform ((if w ^. WB.value then 1 else 0) :: Int)

    label <- do
        (mesh, width) <-  buildLabel $ w ^. WB.label
        position      <-  position mesh
        position   `setY` (5.0 + size ^. y / 2.0)
        position   `setX` 4.0
        position   `setZ` 0.001
        return mesh

    background <- do
        let (vs, fs) = loadShaders "toggle"
        sizeU     <- buildVector2 (size ^. x) (size ^. y) >>= toUniform
        objectId  <- buildVector3 ((fromIntegral $ oid `mod` 256) / 255.0) ((fromIntegral $ oid `div` 256) / 255.0) 0.0 >>= toUniform
        geom      <- buildPlaneGeometry 1.0 1.0
        Geometry.translate geom 0.5 0.5 0.0
        material <- buildShaderMaterial vs fs True NormalBlending DoubleSide
        setUniforms material [ (Size     , sizeU    )
                             , (ObjectId , objectId )
                             , (Value    , value    )
                             ]
        mesh     <- buildMesh geom material
        s        <- scale mesh
        s     `setX` (size ^. x)
        s     `setY` (size ^. y)
        return mesh


    group `add` background
    group `add` label

    p <- (mesh group) >>= position
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

    uniforms <- JSObject.create
    JSObject.setProp "value"    (JSObject.getJSRef $ unUniform value) uniforms

    slider <- JSObject.create

    JSObject.setProp "mesh"       (unGroup group)              slider
    JSObject.setProp "label"      label                        slider
    JSObject.setProp "background" background                   slider
    JSObject.setProp "uniforms"   (JSObject.getJSRef uniforms) slider

    return $ Toggle slider

getFromRegistry :: WB.Toggle -> IO Toggle
getFromRegistry b = (getFromRegistryJS widgetId >>= return . Toggle . JSObject.fromJSRef)
    where widgetId = b ^. WB.refId

putToRegistry :: WB.Toggle -> Toggle -> IO ()
putToRegistry b u = putToRegistryJS widgetId (JSObject.getJSRef $ unToggle u)
    where widgetId = b ^. WB.refId

removeFromRegistry :: WB.Toggle -> IO ()
removeFromRegistry b = removeFromRegistryJS $ b ^. WB.refId

setUniform :: Text -> JSRef a -> WB.Toggle -> IO ()
setUniform n v w = do
    bref     <- getFromRegistry w
    uniforms <- JSObject.getProp "uniforms"             (unToggle bref) >>= return .           JSObject.fromJSRef
    uniform  <- JSObject.getProp (lazyTextToJSString n)  uniforms       >>= return . Uniform . JSObject.fromJSRef
    Uniform.setValue uniform v


updateValue :: WB.Toggle -> IO ()
updateValue w = setUniform "value" (toJSInt val) w where
    val = if w ^. WB.value then 1 else 0

instance Clickable WB.Toggle where
    onClick _ toggle = (Just action, toCtxDynamic newToggle) where
        newToggle = toggle & WB.value .~ (not $ toggle ^. WB.value)
        action    = updateValue newToggle

