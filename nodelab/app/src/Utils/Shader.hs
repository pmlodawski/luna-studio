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


test :: IO ()
test = do
    putStrLn "HSProcessing test started."

    let objBall = myBall
        [gw', gh'] = toList $ objBall ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    GLSL.compileGLSL objBall

    putStrLn "HSProcessing test finished."
