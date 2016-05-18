{-# LANGUAGE DataKinds #-}

module Utils.Shader where

import           Prologue                            hiding (Bounded)

import           Control.Lens

import qualified Data.Array.Linear                   as A

import           Data.Array.Linear.Color.Class
import           Graphics.Rendering.GLSL.SDF         (Object, diff, merge,
                                                      object, translate)
import           Graphics.Rendering.GLSL.SDF.Figures -- (ball)
import           Graphics.Shading.Flat
import           Graphics.Shading.Material
import           Graphics.Shading.Pattern

import           Math.Space.Metric.Bounded           -- (Bounded(..))

import qualified Language.GLSL                       as GLSL
import qualified Language.GLSL.Builder               as GLSL



import qualified Graphics.API.Shapes                 as Visualization
import qualified Graphics.API.Materials              as Visualization
import qualified Graphics.API.Transformations        as Visualization
import qualified Graphics.API.Objects                as Visualization

-- test

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

-- lib

toExpr :: Double -> GLSL.Expr
toExpr v = GLSL.FloatConstant $ (realToFrac v :: Float)

createShape :: Visualization.Shape -> Object 2
createShape (Visualization.Square s)      = hyperrectangle (A.vec2 (toExpr s) (toExpr s) :: A.BVec 2 GLSL.Expr)
createShape (Visualization.Rectangle w h) = hyperrectangle (A.vec2 (toExpr w) (toExpr h) :: A.BVec 2 GLSL.Expr)
createShape (Visualization.Circle d)      = ball (toExpr d)

createMtl :: Visualization.Color -> Material (Layer GLSL.Expr)
createMtl (Visualization.Color r g b a) = Material $ [ Fill . Solid $ color4 (toExpr r) (toExpr g) (toExpr b) (toExpr a)]

createObj :: Visualization.Object -> Bounded Float (Object 2)
createObj (Visualization.Object shape color transformation) =
    Bounded (A.vec2 400 400) (createShape shape)
    & material .~ (createMtl color)

createShader :: Visualization.Object -> String
createShader object = fst $ GLSL.compileGLSL $ createObj object

test :: IO ()
test = do
    putStrLn "HSProcessing test started."

    let objBall = myBall
        [gw', gh'] = toList $ objBall ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let (str, u) = GLSL.compileGLSL objBall
    putStrLn str

    putStrLn "HSProcessing test finished."
