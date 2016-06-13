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

import           Prologue      hiding (primitive)
import           GHC.Prim      (Any)
import           Unsafe.Coerce

import           Graphics.API

fromGraphics :: Any -> Graphics
fromGraphics = unsafeCoerce

fromLayer :: Any -> Graphics
fromLayer v = layerToGraphics (unsafeCoerce v :: Layer)

fromGeometry :: Any -> Graphics
fromGeometry v = geometryToGraphics (unsafeCoerce v :: Geometry)

fromGeoComponent :: Any -> Graphics
fromGeoComponent v = geoComponentToGraphics (unsafeCoerce v :: GeoComponent)

fromSurface :: Any -> Graphics
fromSurface v = surfaceToGraphics (unsafeCoerce v :: Surface)

fromShape :: Any -> Graphics
fromShape v = shapeToGraphics (unsafeCoerce v :: Shape)

fromPrimitive :: Any -> Graphics
fromPrimitive v = primitiveToGraphics (unsafeCoerce v :: Primitive)

fromFigure :: Any -> Graphics
fromFigure v = figureToGraphics (unsafeCoerce v :: Figure)

fromMaterial :: Any -> Graphics
fromMaterial v = geometryToGraphics geometry where
    mat      = unsafeCoerce v :: Material
    geometry = Geometry (GeoElem [ShapeSurface (Single (Primitive (Square 1.0) def def))]) def (Just mat)

-- internal

defaultMat :: Maybe Material
defaultMat = Just $ SolidColor 0.6 0.6 0.6 1.0

defaultTrans :: Transformation
defaultTrans = translate def 0.5 0.5

layerToGraphics :: Layer -> Graphics
layerToGraphics = Graphics . pure

geometryToGraphics :: Geometry -> Graphics
geometryToGraphics geometry = Graphics [Layer geometry [defaultTrans]]

geoComponentToGraphics :: GeoComponent -> Graphics
geoComponentToGraphics geoComponent = geometryToGraphics $ Geometry geoComponent def defaultMat

surfaceToGraphics :: Surface -> Graphics
surfaceToGraphics surface = geoComponentToGraphics $ GeoElem [surface]

shapeToGraphics :: Shape -> Graphics
shapeToGraphics shape = surfaceToGraphics $ ShapeSurface shape

primitiveToGraphics :: Primitive -> Graphics
primitiveToGraphics primitive = shapeToGraphics $ Single primitive

figureToGraphics :: Figure -> Graphics
figureToGraphics figure = primitiveToGraphics $ Primitive figure def def
