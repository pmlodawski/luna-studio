{-# LANGUAGE DataKinds #-}

module Utils.Shader (
    ShaderBox(..)
  , createShaderBox
  ) where

import           Prologue                            hiding (Bounded)
import           Development.Placeholders
import           Utils.Vector

import           Control.Lens
import           Data.Maybe                          (catMaybes, fromMaybe)

import qualified Data.Array.Linear                   as A
import           Data.Array.Linear.Color.Class
import           Graphics.Rendering.GLSL.SDF         (Object, diff, merge, intersect, object, translate)
import           Graphics.Rendering.GLSL.SDF.Figures
import           Graphics.Shading.Flat
import           Graphics.Shading.Material
import           Graphics.Shading.Pattern

import           Math.Space.Metric.Bounded

import qualified Language.GLSL                       as GLSL
import qualified Language.GLSL.Builder               as GLSL

import qualified Graphics.API                        as G


type Size = Vector2 Double
type Shader = String

data ShaderBox = ShaderBox { _shader :: Shader
                           , _size   :: Size
                           } deriving (Show, Eq)

makeLenses ''ShaderBox

-- helpers

toFloat :: Double -> Float
toFloat = realToFrac

toDouble :: Float -> Double
toDouble = realToFrac

toExpr :: Double -> GLSL.Expr
toExpr = GLSL.FloatConstant . toFloat

-- object helpers

mergeObjects :: [Object 2] -> Maybe (Object 2)
mergeObjects (object:objects@(_:_)) = Just $ foldl merge object objects
mergeObjects (object:_)             = Just object
mergeObjects []                     = Nothing

-- TODO: fix behaviour
transObject :: Double -> Double -> Object 2 -> Object 2
transObject 0.0 0.0 object = object
transObject dx  dy  object = translate tr object where
    tr = fromListUnsafe [toExpr (-dx), toExpr (-dy), toExpr 0.0] :: A.BVec 3 GLSL.Expr

-- calc object

fromMaterial :: G.Material -> Material (Layer GLSL.Expr)
fromMaterial (G.SolidColor r g b a) = Material $ [ Fill . Solid $ color4 (toExpr r) (toExpr g) (toExpr b) (toExpr a) ]

fromFigure :: G.Figure -> Object 2
fromFigure (G.Square s)      = hyperrectangle (A.vec2 (toExpr s) (toExpr s) :: A.BVec 2 GLSL.Expr)
fromFigure (G.Rectangle w h) = hyperrectangle (A.vec2 (toExpr w) (toExpr h) :: A.BVec 2 GLSL.Expr)
fromFigure (G.Circle d)      = ball (toExpr d)

fromPrimitive :: G.Primitive -> Object 2
fromPrimitive (G.Primitive figure (G.Point2 dx dy) attr) = transObject dx dy $ fromFigure figure

fromShape :: G.Shape -> Object 2
fromShape (G.Shape     primitive)     = fromPrimitive primitive
fromShape (G.Merge     shape1 shape2) = merge     (fromShape shape1) (fromShape shape2)
fromShape (G.Subtract  shape1 shape2) = diff      (fromShape shape1) (fromShape shape2)
fromShape (G.Intersect shape1 shape2) = intersect (fromShape shape1) (fromShape shape2)

fromSurface :: G.Surface -> Object 2
fromSurface (G.ShapeSurface shape) = fromShape shape
fromSurface G.PolygonSurface       = $notImplemented
fromSurface G.NumbsSurface         = $notImplemented

fromSurfaces :: [G.Surface] -> Maybe (Object 2)
fromSurfaces surfaces = mergeObjects $ fromSurface <$> surfaces

fromGeoComponent :: G.GeoComponent -> Maybe (Object 2)
fromGeoComponent (G.GeoElem  surfaces)   = fromSurfaces surfaces
fromGeoComponent (G.GeoGroup geometries) = fromGeometries geometries

fromGeometry :: G.Geometry -> Maybe (Object 2)
fromGeometry (G.Geometry geoComp trans matMay) = go <$> fromGeoComponent geoComp where
    (G.Transformation _ _ dx dy _ _) = trans
    go :: Object 2 -> Object 2
    go = appMat . transObject dx dy
    appMat :: Object 2 -> Object 2
    appMat object = case matMay of
        Just mat -> object & material .~ (fromMaterial mat)
        Nothing  -> object

fromGeometries :: [G.Geometry] -> Maybe (Object 2)
fromGeometries geometries = mergeObjects . catMaybes $ fromGeometry <$> geometries

createShader :: Size -> Maybe (Object 2) -> Shader
createShader size objectMay = fromMaybe "" $ compileObject <$> objectMay where
    compileObject :: Object 2 -> Shader
    compileObject object = fst $ GLSL.compileGLSL $ Bounded (toBound size) object

-- size calculation

defSize = Vector2 2.0 2.0

maxSizes :: Size -> Size -> Size
maxSizes (Vector2 sx1 sy1) (Vector2 sx2 sy2) = Vector2 (max sx1 sx2) (max sy1 sy2)

minSizes :: Size -> Size -> Size
minSizes (Vector2 sx1 sy1) (Vector2 sx2 sy2) = Vector2 (min sx1 sx2) (min sy1 sy2)

maxSizesList :: [Size] -> Size
maxSizesList (size:sizes@(x:xs)) = maxSizes size $ maxSizesList sizes
maxSizesList (size:_)            = size
maxSizesList []                  = defSize

-- getBound :: G.Figure -> A.BVec 2 Float
-- getBound shape = let Vector2 w h = getSize shape in A.vec2 (toFloat w) (toFloat h)

toBound :: Size -> A.BVec 2 Float
toBound (Vector2 x y) = A.vec2 (toFloat x) (toFloat y)

calcFigureSize :: G.Figure -> Size
calcFigureSize (G.Square s)      = Vector2 s s
calcFigureSize (G.Rectangle w h) = Vector2 w h
calcFigureSize (G.Circle d)      = Vector2 (2.0 * d) (2.0 * d)

calcPrimitiveSize :: G.Primitive -> Size
calcPrimitiveSize (G.Primitive figure (G.Point2 dx dy) attr) = calcFigureSize figure

calcShapeSize :: G.Shape -> Size
calcShapeSize (G.Shape     primitive)     = calcPrimitiveSize primitive
calcShapeSize (G.Merge     shape1 shape2) = maxSizes (calcShapeSize shape1) (calcShapeSize shape2)
calcShapeSize (G.Subtract  shape1 shape2) = maxSizes (calcShapeSize shape1) (calcShapeSize shape2)
calcShapeSize (G.Intersect shape1 shape2) = minSizes (calcShapeSize shape1) (calcShapeSize shape2)

calcSurfaceSize :: G.Surface -> Size
calcSurfaceSize (G.ShapeSurface shape) = calcShapeSize shape
calcSurfaceSize G.PolygonSurface       = $notImplemented
calcSurfaceSize G.NumbsSurface         = $notImplemented

calcSurfacesSize :: [G.Surface] -> Size
calcSurfacesSize surfaces = maxSizesList $ calcSurfaceSize <$> surfaces

calcGeoCompSize :: G.GeoComponent -> Size
calcGeoCompSize (G.GeoElem  surfaces)   = calcSurfacesSize surfaces
calcGeoCompSize (G.GeoGroup geometries) = maxSizesList $ calcGeometrySize <$> geometries

calcGeometrySize :: G.Geometry -> Size
calcGeometrySize (G.Geometry geoComp trans matMay) = calcGeoCompSize geoComp

-- -- --

createShaderBox :: G.Geometry -> ShaderBox
createShaderBox geometry = ShaderBox (createShader size objMay) size
    where size   = calcGeometrySize geometry
          objMay = fromGeometry geometry

-- tests

test :: IO ()
test = do
    let geometry  = G.Geometry geoComp trans justMat
        trans     = def
        justMat   = Just $ G.SolidColor 1.0 0.0 0.0 1.0
        geoComp   = G.GeoElem [surface]
        surface   = G.ShapeSurface shape
        shape     = G.Shape primitive
        primitive = G.Primitive figure def def
        figure    = G.Square 0.25
        ShaderBox shaderTxt (Vector2 w h) = createShaderBox geometry
    return ()





-- ====== old test (TODO: remove) ====== --

mtl1     = Material $ [ Fill            . Solid $ color4 0.7 0.2 0.2 1.0
                      , Border 10.0     . Solid $ color4 0.0 1.0 0.0 1.0
                      , Shadow 10.0 2.0 . Solid $ color4 0.0 0.0 0.0 0.2
                      ] :: Material (Layer GLSL.Expr)

mtl2     = Material $ [ Fill            . Solid $ color4 0.6 0.6 0.6 1.0
                      ] :: Material (Layer GLSL.Expr)

mtl3     = Material $ [ Fill            . Solid $ color4 0.3 0.3 0.3 1.0
                      ] :: Material (Layer GLSL.Expr)


myBall :: Bounded Float (Object 2)
myBall = Bounded (A.vec2 400 400) (ball 100.0)
       & material .~ mtl1

testRaw :: IO ()
testRaw = do
    putStrLn "HSProcessing test started."

    let objBall = myBall
        [gw', gh'] = toList $ objBall ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let (str, u) = GLSL.compileGLSL objBall
    putStrLn str

    putStrLn "HSProcessing test finished."


