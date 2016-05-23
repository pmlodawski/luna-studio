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

import qualified Graphics.API                        as G

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

toFloat :: Double -> Float
toFloat = realToFrac

toDouble :: Float -> Double
toDouble = realToFrac

toExpr :: Double -> GLSL.Expr
toExpr = GLSL.FloatConstant . toFloat

createShape :: G.Shape -> Object 2
createShape (G.Square s)      = hyperrectangle (A.vec2 (toExpr s) (toExpr s) :: A.BVec 2 GLSL.Expr)
createShape (G.Rectangle w h) = hyperrectangle (A.vec2 (toExpr w) (toExpr h) :: A.BVec 2 GLSL.Expr)
createShape (G.Circle d)      = ball (toExpr d)

getSize :: G.Shape -> (Double, Double)
getSize (G.Square s)      = (s, s)
getSize (G.Rectangle w h) = (w, h)
getSize (G.Circle d)      = (2.0 * d, 2.0 * d)

getBound :: G.Shape -> A.BVec 2 Float
getBound shape = let (w, h) = getSize shape in A.vec2 (toFloat w) (toFloat h)

createMtl :: G.Color -> Material (Layer GLSL.Expr)
createMtl (G.Color r g b a) = Material $ [ Fill . Solid $ color4 (toExpr r) (toExpr g) (toExpr b) (toExpr a)]

createComponent :: G.Component -> Bounded Float (Object 2)
createComponent component@(G.Component shape color) =
    Bounded (getBound shape) (createObject component)

createObject :: G.Component -> Object 2
createObject (G.Component shape color) = (createShape shape) & material .~ (createMtl color)

createCompositeComponent :: [G.Component] -> Maybe (Object 2)
createCompositeComponent components = createCompositeObject $ fmap createObject components

createCompositeObject :: [Object 2] -> Maybe (Object 2)
createCompositeObject (object:objects@(x:xs)) = Just $ foldl merge object objects
createCompositeObject (object:_) = Just object
createCompositeObject [] = Nothing

-- TODO: compile all components
createShader :: G.Shader -> (String, (Double, Double))
createShader (G.Shader (component:components)) = createShader' component
createShader _ = ("", (0.0, 0.0))

createShader' :: G.Component -> (String, (Double, Double))
createShader' component@(G.Component shape _) = (fst $ GLSL.compileGLSL $ createComponent component, getSize shape)


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
