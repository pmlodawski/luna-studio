---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Geom2D.QuadraticBezier.Conversion
import           Flowbox.Geom2D.Rasterizer                       hiding (makePoints, makeSegments)
import           Flowbox.Graphics.Mockup                         (saveImageLuna)
import           Flowbox.Math.Matrix                             ((:.) (..), DIM2, Matrix (..), Matrix2, Z (..))
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Prelude
import qualified Flowbox.Prelude                                 as P


import           Data.Array.Accelerate                           as A
import           Math.Coordinate.Cartesian                       (Point2 (..))



combine :: [QuadraticBezier Double] -> Exp DIM2 -> Exp Double
combine quads (A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) =
    (distanceFromQuadratics (A.lift $ Point2 x y) quad') / 30
        where quad' = A.use $ A.fromList (Z:. P.length quads) quads

main = do
    --let cubic = CubicBezier (Point2 100 100) (Point2 100 400) (Point2 100 400) (Point2 400 400)

    --print $ approximateCubicWithQuadratic' 5 0.001 cubic

    --print $ convertCubicsToQuadratics 5 0.001 [cubic]

    let cubic  = CubicBezier (Point2 50 650) (Point2 50 250) (Point2 250 50) (Point2 650 50)
        cubic2 = CubicBezier (Point2 50 650) (Point2 450 650) (Point2 650 450) (Point2 650 50)
        quad   = QuadraticBezier (Point2 50 650) (Point2 50 50) (Point2 650 50)
        quad2  = QuadraticBezier (Point2 50 650) (Point2 650 650) (Point2 650 50)
        matQ   = M.generate (A.index2 700 700) $ combine [quad, quad2]
        matC   = M.generate (A.index2 700 700) $ combine $ convertCubicsToQuadratics 5 0.001 [cubic, cubic2]
        matC2  = M.generate (A.index2 700 700) $ combine $ (approximateCubicWithQuadratic' 5 0.001 cubic) P.++ (approximateCubicWithQuadratic' 5 0.001 cubic2)
        imgQ   = matrixToImage matQ
        imgC   = matrixToImage matC
        imgC2  = matrixToImage matC2

    print $ (approximateCubicWithQuadratic' 5 0.001 cubic) P.++ (approximateCubicWithQuadratic' 5 0.001 cubic2)
    --print $ (approximateCubicWithQuadratic' 5 0.001 cubic2)

    saveImageLuna "foo.png" imgQ
    saveImageLuna "foo1.png" imgC
    saveImageLuna "foo2.png" imgC2


    return ()
