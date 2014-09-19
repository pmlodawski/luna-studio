---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

module Flowbox.Graphics.Composition.Generators.Filter where

import Flowbox.Graphics.Composition.Generators.Shape
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Stencil
import Flowbox.Graphics.Composition.Generators.Matrix

import           Flowbox.Graphics.Prelude          as P hiding (filter)
import           Flowbox.Math.Matrix               as M hiding (stencil)
import qualified Flowbox.Math.Matrix               as M (stencil)
import           Data.Array.Accelerate             as A hiding (filter, stencil, constant)
import qualified Data.Array.Accelerate.CUDA.Thrust as Thrust

import Math.Space.Space
import Math.Coordinate.Cartesian                (Point2(..))


-- == Filter datatype ==

data Filter a = Filter { window :: a
                       , apply :: a -> a
                       }

instance (Condition a, Ord a, Num a) => Num (Filter a) where
    Filter w1 a1 + Filter w2 a2 = Filter (w1 `max` w2) $ \t -> a1 t + a2 t
    Filter w1 a1 - Filter w2 a2 = Filter (w1 `max` w2) $ \t -> a1 t - a2 t
    Filter w1 a1 * Filter w2 a2 = Filter (w1 `max` w2) $ \t -> a1 t * a2 t
    negate (Filter w a) = Filter w $ \t -> negate (a t)
    abs (Filter w a)    = Filter w $ \t -> abs (a t)
    signum (Filter w a) = Filter w $ \t -> signum (a t)
    fromInteger x       = Filter 1 $ \t -> fromInteger x

-- == Helper functions ==

toMatrix :: (Elt a, IsFloating a) => Grid (Exp Int) -> Filter (Exp a) -> Matrix2 a
toMatrix (Grid sizex sizey) filter = M.generate (A.index2 sizey sizex) $ \(A.unlift -> Z :. y :. x :: EDIM2) ->
    let sx = 2 * window filter / A.fromIntegral sizex
        sy = 2 * window filter / A.fromIntegral sizey
        vx = apply filter $ A.fromIntegral (x - sizex `div` 2) * sx
        vy = apply filter $ A.fromIntegral (y - sizey `div` 2) * sy
    in vx * vy

normalize :: (Elt a, IsFloating a) => Matrix2 a -> Matrix2 a
normalize kern = M.map (/ksum) kern
    where ksum = M.the $ M.sum kern

-- == Windowed filters ==

box :: (Num a, Condition a, Ord a) => Filter a
box = Filter 1 $ \(abs -> t) -> if' (t <= 1) 1 0

-- TODO: Find the name
basic :: (Num a, Condition a, Ord a) => Filter a
basic = Filter 1 $ \(abs -> t) -> if' (t < 1) ((2 * t - 3) * t * t + 1) 0


triangle :: (Num a, Condition a, Ord a) => Filter a
triangle = Filter 1 $ \(abs -> t) -> if' (t < 1) (1 - t) 0

bell :: (Fractional a, Condition a, Ord a) => Filter a
bell = Filter 1.5 $ \(abs -> t) -> if' (t < 0.5) (0.75 - t * t) 
                                 $ if' (t < 1.5) (0.5 * ((t - 1.5) * (t - 1.5)))
                                 0.0

bspline :: (Fractional a, Condition a, Ord a) => Filter a
bspline = Filter 2.0 $ \(abs -> t) -> if' (t < 1.0) ((0.5 * t * t * t) - t * t + (2.0 / 3.0))
                                    $ if' (t < 2.0) ((1.0 / 6.0) * ((2 - t) * (2 - t) * (2 - t)))
                                    0.0

lanczos :: (Floating a, Condition a, Ord a) => a -> Filter a
lanczos a = Filter a $ \(abs -> t) -> if' (t <=  1e-6) 1.0 
                                    $ if' (t < a) ((a * sin (pi * t) * sin (pi * t / a)) / (pi * pi * t * t))
                                    0.0

lanczos2 :: (Floating a, Condition a, Ord a) => Filter a
lanczos2 = lanczos 2

lanczos3 :: (Floating a, Condition a, Ord a) => Filter a
lanczos3 = lanczos 3

polynomial :: (Fractional a, Condition a, Ord a) => a -> a -> Filter a
polynomial b c = Filter 2.0 $ \(abs -> t) -> (/6.0) $ if' (t < 1.0) (((12.0 - 9.0 * b - 6.0 * c) * (t * t * t)) + ((-18.0 + 12.0 * b + 6.0 * c) * t * t) + (6.0 - 2 * b))
                                                    $ if' (t < 2.0) (((-1.0 * b - 6.0 * c) * (t * t * t)) + ((6.0 * b + 30.0 * c) * t * t) + ((-12.0 * b - 48.0 * c) * t) + (8.0 * b + 24 * c))
                                                    0.0

mitchell :: (Fractional a, Condition a, Ord a) => Filter a
mitchell = polynomial (1.0 / 3.0) (1.0 / 3.0)

catmulRom :: (Fractional a, Condition a, Ord a) => Filter a
catmulRom = polynomial 0.0 0.5

gauss :: (Floating a, Condition a) => a -> Filter a
gauss sigma = Filter ((10.0 / 3.0) * sigma) $ \t -> exp (-(t ** 2) / (2 * sigma * sigma)) / (sigma * sqrt pi)

dirac :: (Floating a, Condition a) => a -> Filter a
dirac sigma = Filter 1.0 $ \(abs -> t) -> exp (- (t * t) / (sigma * sigma)) / (sigma * sqrt pi)

-- == Non separable filters ==

laplacian :: (Elt a, IsFloating a) => Exp a -> Exp a -> Grid (Exp Int) -> Matrix2 a
laplacian cross side (fmap (1+).(2*) -> Grid sizex sizey) = M.generate (A.index2 sizey sizex) $ \(A.unlift -> Z :. y :. x :: EDIM2) ->
    let center = A.fromIntegral (sizex + sizey - 2) * cross + A.fromIntegral ((sizex - 1) * (sizey - 1)) * side
    in  if' (x == sizex `div` 2) 
            (if' (y == sizey `div` 2) (-center) cross) 
            (if' (y == sizey `div` 2) cross     side)

-- == Constant sized kernels ==

prewitt :: (Elt a, IsFloating a) => Matrix2 a
prewitt = M.fromList (Z :. 3 :. 3) [ -1, 0, 1
                                   , -1, 0, 1
                                   , -1, 0, 1
                                   ]

sobel :: (Elt a, IsFloating a) => Matrix2 a
sobel = M.fromList (Z :. 3 :. 3) [ -1, 0, 1
                                 , -2, 0, 2
                                 , -1, 0, 1
                                 ]

scharr :: (Elt a, IsFloating a) => Matrix2 a
scharr = M.fromList (Z :. 3 :. 3) [  -3, 0, 3
                                  , -10, 0, 10
                                  ,  -3, 0, 3
                                  ]


-- == General convolutions ==

convolve :: (IsNum a, Elt a) => (Point2 c -> Point2 (Exp Int) -> Point2 b)
         -> Matrix2 a -> CartesianGenerator b (Exp a) -> CartesianGenerator c (Exp a)
convolve mode kernel = stencil mode (unsafeFromMatrix kernel) (+) 0

filter :: (Elt a, IsNum a) => Exp Int -> Matrix2 a -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
filter scatter = convolve $ \point offset -> point + pure scatter * offset

-- == Morphological filters

dilate :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
dilate size = stencil (+) (constant size 1) max (-1e20)

erode :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
erode size = stencil (+) (constant size 1) min 1e20

opening :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
opening size = erode size . dilate size

closing :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
closing size = dilate size . erode size

-- == Median ==

median :: forall a . (A.Stencil A.DIM2 a (A.Stencil3x3 a), A.IsFloating a)
       => M.Matrix2 a -> M.Matrix2 a
median = M.stencil stencil A.Mirror
    where stencil :: A.Stencil3x3 a -> A.Exp a
          stencil ((v0, v1, v2)
                  ,(v3, v4, v5)
                  ,(v6, v7, v8)) = A.the $ middle $ Thrust.sort (unit1 v0 A.++ unit1 v1 A.++ unit1 v2
                                                            A.++ unit1 v3 A.++ unit1 v4 A.++ unit1 v5
                                                            A.++ unit1 v6 A.++ unit1 v7 A.++ unit1 v8)

stencilTest :: forall a . (A.Stencil A.DIM2 a (A.Stencil3x3 a), A.IsFloating a)
       => M.Matrix2 a -> M.Matrix2 a
stencilTest = M.stencil stencil A.Mirror
    where stencil ((v0, v1, v2)
                  ,(v3, v4, v5)
                  ,(v6, v7, v8)) = (v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8) / 9

unit1 :: (A.Elt a) => A.Exp a -> A.Acc (A.Array A.DIM1 a)
unit1 = A.reshape (A.index1 1) . A.unit

middle :: (A.Elt a, A.IsNum a, A.IsFloating a) => A.Acc (A.Vector a) -> A.Acc (A.Scalar a)
middle vec = A.unit ((len `mod` 2) == 0 A.? ( vec A.! A.index1 ((len + 1) `div` 2)
                                               , (vec A.! A.index1 (len `div` 2)
                                                 + vec A.! A.index1 ((len `div` 2) + 1))
                                                 / 2
                                               ))
    where len = A.unindex1 $ A.shape vec

sort :: (A.IsNum a, A.Elt a) => M.Matrix2 a -> M.Matrix2 a
sort m = Delayed $ Thrust.sort $ accMatrix m
