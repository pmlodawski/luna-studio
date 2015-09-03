{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Widget.Node where

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
import           JS.Bindings
import           Utils.Vector
import           ThreeJS.Registry as Registry
import qualified Object.Widget.Node as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           ThreeJS.Uniform (Uniform(..), UniformMap(..), toUniform)
import qualified ThreeJS.Uniform as Uniform
import           ThreeJS.Widget.Common


newtype Node = Node { unNode :: JSObject.Object }

instance Object Node where
    mesh b = (JSObject.getProp "mesh" $ unNode b) :: IO Mesh

instance Registry.UIWidget Node where
    wrapWidget   = Node
    unwrapWidget = unNode

instance UIWidgetBinding Model.Node Node where
    build = undefined


containerMesh :: Node -> IO Mesh
containerMesh b = (JSObject.getProp "expandedNode" $ unNode b) :: IO Mesh


instance Container Node where
    add    m o = do mo <- mesh o
                    mm <- containerMesh m
                    addJS mm mo
    remove m o = do mo <- mesh o
                    mm <- containerMesh m
                    removeJS mm mo
