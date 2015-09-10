{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Button where

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
import           ThreeJS.Text (calculateTextWidth)
import qualified ThreeJS.Geometry as Geometry
import           Utils.Vector
import           JS.Config as Config
import           ThreeJS.Registry
import qualified Object.Widget.Button as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           Object.UITypes
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import qualified ThreeJS.Registry as Registry
import           ThreeJS.Widget.Common


newtype Button = Button { unButton :: JSObject.Object }

data Uniforms = Color | State  deriving (Show)
instance Uniform.UniformKey Uniforms

data Components = Background | Label deriving (Show)
instance Registry.ComponentKey Components

instance Object Button where
    mesh b = (JSObject.getProp "mesh" $ unButton b) :: IO Mesh

instance UIWidget Button where
    wrapWidget   = Button
    unwrapWidget = unButton


buttonPadding = 20
buttonWidth text = Config.fontSize * (calculateTextWidth text) + 2 * buttonPadding


instance UIWidgetBinding Model.Button Button where
    build oid model = do
        group    <- buildGroup
        state    <- toUniform (0 :: Int)

        background <- do
            let (vs, fs) = loadShaders "button"
            color     <- buildVector4 1.0 0.0 1.0 1.0 >>= toUniform
            sizeU     <- toUniform $ objectSize model
            objectId  <- objectIdToUniform oid

            geom      <- buildNormalizedPlaneGeometry
            material  <- buildShaderMaterial vs fs True NormalBlending DoubleSide

            setUniforms material [ (Size     , sizeU     )
                                 , (ObjectId , objectId  )
                                 ]
            setUniforms material [ (Color, color)
                                 , (State, state)
                                 ]

            mesh      <- buildMesh geom material
            scaleBy (objectSize model) mesh
            pos       <- position mesh
            pos `setZ` (-0.0001)

            return mesh

        label <- do
            (mesh, width) <- buildLabel 1.0 AlignCenter (model ^. Model.label)
            moveBy (Vector2 (model ^. Model.size . x / 2.0) (5.0 + model ^. Model.size . y / 2.0)) mesh
            return mesh


        group `add` background
        group `add` label

        mesh   <- mesh group
        moveTo (model ^. Model.pos) mesh


        (button, uniforms) <- buildSkeleton mesh
        Uniform.setUniform uniforms State state

        addComponents button [ (Label, label)
                             , (Background, background)
                             ]

        return button

updateState :: WidgetId -> Model.Button -> IO ()
updateState oid model = do
    updateUniformValue State (toJSInt $ fromEnum $ model ^. Model.state) oid

instance HandlesMouseOver Model.Button where
    onMouseOver file model = (Just action, toCtxDynamic newModel) where
                  action   = updateState (file ^. objectId) newModel
                  newModel = model & Model.state .~ Model.Focused

instance HandlesMouseOut Model.Button where
    onMouseOut  file model = (Just action, toCtxDynamic newModel) where
                  action   = updateState (file ^. objectId) newModel
                  newModel = model & Model.state .~ Model.Normal

instance Clickable Model.Button where
    onClick pos file model = (Just action, toCtxDynamic newModel) where
                  action   = updateState (file ^. objectId) newModel
                  newModel = model & Model.state .~ Model.Pressed

instance DblClickable Model.Button where
    onDblClick pos  file model = (Just action, toCtxDynamic newModel) where
                  action   = updateState (file ^. objectId) newModel
                  newModel = model & Model.state .~ Model.Disabled

