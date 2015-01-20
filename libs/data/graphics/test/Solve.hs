---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Data.Array.Accelerate                           as A

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.Rasterizer                       hiding (makePoints, makeSegments)
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Math.Matrix                             ((:.) (..), DIM2, Matrix (..), Matrix2, Z (..))
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Graphics.Mockup (saveImageLuna)
import           Flowbox.Prelude




combine :: Exp (QuadraticBezier Double) -> Exp DIM2 -> Exp Double
combine quad (A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) = 
    (distanceFromQuadratic (A.lift $ Point2 x y) quad) / 30

combine2 :: Exp (QuadraticBezier Double) -> Exp DIM2 -> Exp Double
combine2 quad (A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) =
    (distanceFromQuadratics (A.lift $ Point2 x y) quad') / 30
        where
            quad' = A.fill (A.index1 1) quad

combine3 :: [QuadraticBezier Double] -> Exp DIM2 -> Exp Double
combine3 quads (A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) =
    (distanceFromQuadratics (A.lift $ Point2 x y) quad') / 30
        where quad' = A.use $ A.fromList (Z:.2) quads

main = do
    let quad  = QuadraticBezier (Point2 0 0) (Point2 500 0) (Point2 500 500) :: QuadraticBezier Double
        quad2 = QuadraticBezier (Point2 0 0) (Point2 0 500) (Point2 500 500) :: QuadraticBezier Double
        mat :: Matrix2 Double
        mat  = M.generate (A.index2 500 500) $ combine $ A.lift quad
        img  = matrixToImage mat
        mat1  = M.generate (A.index2 500 500) $ combine $ A.lift quad2
        img1  = matrixToImage mat1
        mat2 = M.generate (A.index2 500 500) $ combine2 $ A.lift quad
        img2 = matrixToImage mat2
        mat3 = M.generate (A.index2 500 500) $ combine3 [quad, quad2]
        img3 = matrixToImage mat3

    saveImageLuna "foo.png" img
    saveImageLuna "foo1.png" img1
    saveImageLuna "foo2.png" img2
    saveImageLuna "foo3.png" img3

    return ()