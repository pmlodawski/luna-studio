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

import           Data.Array.Accelerate                           as A

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Geom2D.Rasterizer                       hiding (makePoints, makeSegments)
import           Flowbox.Graphics.Mockup                         (saveImageLuna)
import           Flowbox.Math.Matrix                             ((:.) (..), DIM2, Matrix (..), Matrix2, Z (..))
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Prelude
import qualified Flowbox.Prelude                                 as P

import           Data.Array.Accelerate.CUDA




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
        quadS  = QuadraticBezier (Point2 10.0 10.0) (Point2 216.6 10.0) (Point2 423.3 10.0) :: QuadraticBezier Double
        quadS2 = fmap (/(10000)) . (fmap P.fromIntegral) . (fmap P.round) . (fmap (*(10000 :: Double))) $ QuadraticBezier (Point2 10.0 10.0) (Point2 216.66666666666666 10.0) (Point2 423.33333333333337 10.0) :: QuadraticBezier Double
        quadS3 = QuadraticBezier (Point2 10.0 10.0) (Point2 320.0 10.0) (Point2 630.0 10.0) :: QuadraticBezier Double
        quadS4 = QuadraticBezier (Point2 10.0 10.0) (Point2 216.66666666666666 10.0) (Point2 423.3              10.0) :: QuadraticBezier Double
        quadS5 = QuadraticBezier (Point2 293.366284 293.596489) (Point2 363.9906950000002 169.23332700000003) (Point2 434.615106 44.870165) :: QuadraticBezier Double
        mat :: Matrix2 Double
        mat  = M.generate (A.index2 500 500) $ combine $ A.lift quad
        img  = matrixToImage mat
        mat1  = M.generate (A.index2 500 500) $ combine $ A.lift quad2
        img1  = matrixToImage mat1
        mat2 = M.generate (A.index2 500 500) $ combine2 $ A.lift quad
        img2 = matrixToImage mat2
        mat3 = M.generate (A.index2 500 500) $ combine3 [quad, quad2]
        img3 = matrixToImage mat3

    print quadS2

    putStrLn "testing distanceFromQuadratics 1"
    saveImageLuna "foo.png" img
    putStrLn "testing distanceFromQuadratics 2"
    saveImageLuna "foo1.png" img1
    putStrLn "testing distanceFromQuadratics 3"
    saveImageLuna "foo2.png" img2
    putStrLn "testing distanceFromQuadratics 4"
    saveImageLuna "foo3.png" img3

    putStrLn "testing distanceFromQuadratics Straight Line --> fooD1.png"
    let matS = M.generate (A.index2 500 500) $ combine2 $ A.lift quadS
        imgS = matrixToImage matS

    saveImageLuna "fooD1.png" imgS

    putStrLn "testing distanceFromQuadratics Straight Line 2 --> fooD2.png"
    let matS = M.generate (A.index2 500 500) $ combine2 $ A.lift quadS2
        imgS = matrixToImage matS

    saveImageLuna "fooD2.png" imgS

    putStrLn "testing distanceFromQuadratics Straight Line 3 --> fooD3.png"
    let matS = M.generate (A.index2 500 500) $ combine2 $ A.lift quadS3
        imgS = matrixToImage matS

    saveImageLuna "fooD3.png" imgS

    let matS = M.generate (A.index2 30 30) $ combine2 $ A.lift quadS3
    --print $ M.compute run matS

    putStrLn "testing distanceFromQuadratics Straight Line 4 --> fooD4.png"
    let matS = M.generate (A.index2 500 500) $ combine2 $ A.lift quadS4
        imgS = matrixToImage matS

    saveImageLuna "fooD4.png" imgS

    putStrLn "testing distanceFromQuadratics GUI example 5 --> fooD5.png"
    let matS = M.generate (A.index2 500 500) $ combine2 $ A.lift quadS5
        imgS = matrixToImage matS

    saveImageLuna "fooD5.png" imgS



    return ()
