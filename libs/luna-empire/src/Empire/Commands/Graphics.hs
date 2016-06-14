module Empire.Commands.Graphics
    ( fromGraphics
    , fromLayer
    , fromGeometry
    , fromGeoComponent
    , fromSurface
    , fromShape
    , fromPrimitive
    , fromFigure
    , fromMaterial
    ) where

import           Prologue
import           GHC.Prim      (Any)
import           Unsafe.Coerce

import           Graphics.API

fromGraphics, fromLayer, fromGeometry, fromGeoComponent, fromSurface,
    fromShape, fromPrimitive, fromFigure, fromMaterial :: Any -> Graphics

fromGraphics       =                                             unsafeCoerce
fromLayer        v = convert                                    (unsafeCoerce v :: Layer)
fromGeometry     v = convert . geometryToLayer'               $ (unsafeCoerce v :: Geometry)
fromGeoComponent v = convert . geoComponentToLayer'           $ (unsafeCoerce v :: GeoComponent)
fromSurface      v = convert . geoComponentToLayer' . convert $ (unsafeCoerce v :: Surface)
fromShape        v = convert . geoComponentToLayer' . convert $ (unsafeCoerce v :: Shape)
fromPrimitive    v = convert . geoComponentToLayer' . convert $ (unsafeCoerce v :: Primitive)
fromFigure       v = convert . geoComponentToLayer' . convert $ (unsafeCoerce v :: Figure)
fromMaterial     v = convert . geometryToLayer'               $ geometry where
    mat      = unsafeCoerce v :: Material
    geometry = Geometry (GeoElem [ShapeSurface (Shape (Primitive (Square 1.0) def def))]) def (Just mat)

-- internal

defaultMat :: Maybe Material
defaultMat = Just $ SolidColor 0.6 0.6 0.6 1.0

defaultTrans :: Transformation
defaultTrans = translate 0.5 0.5 def

geometryToLayer' :: Geometry -> Layer
geometryToLayer' geometry = Layer geometry [defaultTrans]

geoComponentToGeometry' :: GeoComponent -> Geometry
geoComponentToGeometry' geoComponent = Geometry geoComponent def defaultMat

geoComponentToLayer' :: GeoComponent -> Layer
geoComponentToLayer' = geometryToLayer' . geoComponentToGeometry'
