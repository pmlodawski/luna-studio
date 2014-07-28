---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Composition.Generators.Filter where

import Flowbox.Graphics.Composition.Generators.Constant
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Stencil
import Flowbox.Graphics.Composition.Generators.Matrix

import Flowbox.Prelude       as P hiding ((<*), filter)
import Flowbox.Math.Matrix   as M hiding (stencil)
import Data.Array.Accelerate as A hiding (filter, stencil, constant)

import Math.Space.Space
import Math.Coordinate.Cartesian                (Point2(..))
import Linear.V2


-- == Filter datatype ==

data Filter a = Filter { window :: Exp a
                       , apply :: Exp a -> Exp a
                       }


-- == Helper functions ==

toMatrix :: (Elt a, IsFloating a) => Grid (Exp Int) -> Filter a -> Matrix2 a
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

box :: (Elt a, IsFloating a) => Filter a
box = Filter 1 $ \(abs -> t) -> A.cond (t <=* 1.0) 1.0 0.0

-- TODO: Find the name
basic :: (Elt a, IsFloating a) => Filter a
basic = Filter 1.0 $ \(abs -> t) -> A.cond (t <* 1.0) ((2.0 * t - 3.0) * t * t + 1.0) 0.0


triangle :: (Elt a, IsFloating a) => Filter a
triangle = Filter 1.0 $ \(abs -> t) -> A.cond (t <* 1.0) (1.0 - t) 0.0

bell :: (Elt a, IsFloating a) => Filter a
bell = Filter 1.5 $ \(abs -> t) -> A.cond (t <* 0.5) (0.75 - t * t) 
                                 $ A.cond (t <* 1.5) (0.5 * ((t - 1.5)*(t - 1.5)))
                                 0.0

bspline :: (Elt a, IsFloating a) => Filter a
bspline = Filter 2.0 $ \(abs -> t) -> A.cond (t <* 1.0) ((0.5 * t * t * t) - t * t + (2.0 / 3.0))
                                    $ A.cond (t <* 2.0) ((1.0 / 6.0) * ((2 - t) * (2 - t) * (2 - t)))
                                    0.0

lanczos :: (Elt a, IsFloating a) => Exp a -> Filter a
lanczos a = Filter a $ \(abs -> t) -> A.cond (t <=*  1e-6) 1.0 
                                    $ A.cond (t <* a) ((a * sin (pi * t) * sin (pi * t / a)) / (pi * pi * t * t))
                                    0.0

lanczos2 :: (Elt a, IsFloating a) => Filter a
lanczos2 = lanczos 2

lanczos3 :: (Elt a, IsFloating a) => Filter a
lanczos3 = lanczos 3

polynomial :: (Elt a, IsFloating a) => Exp a -> Exp a -> Filter a
polynomial b c = Filter 2.0 $ \(abs -> t) -> (/6.0) $ A.cond (t <* 1.0) (((12.0 - 9.0 * b - 6.0 * c) * (t * t * t)) + ((-18.0 + 12.0 * b + 6.0 * c) * t * t) + (6.0 - 2 * b))
                                                    $ A.cond (t <* 2.0) (((-1.0 * b - 6.0 * c) * (t * t * t)) + ((6.0 * b + 30.0 * c) * t * t) + ((-12.0 * b - 48.0 * c) * t) + (8.0 * b + 24 * c))
                                                    0.0

mitchell :: (Elt a, IsFloating a) => Filter a
mitchell = polynomial (1.0 / 3.0) (1.0 / 3.0)

catmulRom :: (Elt a, IsFloating a) => Filter a
catmulRom = polynomial 0.0 0.5

gauss :: (Elt a, IsFloating a) => Exp a -> Filter a
gauss sigma = Filter ((10.0 / 3.0) * sigma) $ \t -> exp (-(t ** 2) / (2 * sigma * sigma)) / (sigma * sqrt (2 * pi))


-- == Non separable filters ==

laplacian :: (Elt a, IsFloating a) => Exp a -> Exp a -> Grid (Exp Int) -> Matrix2 a
laplacian cross side (fmap (1+).(2*) -> Grid sizex sizey) = M.generate (A.index2 sizey sizex) $ \(A.unlift -> Z :. y :. x :: EDIM2) ->
    let center = (A.fromIntegral $ sizex + sizey - 2) * cross + (A.fromIntegral $ (sizex - 1)*(sizey - 1)) * side
    in  A.cond (x A.==* sizex `div` 2) 
            (A.cond (y A.==* sizey `div` 2) (-center) cross) 
            (A.cond (y A.==* sizey `div` 2) cross     side)

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
         -> Matrix2 a -> Generator b (Exp a) -> Generator c (Exp a)
convolve mode kernel = stencil mode (Grid width height) (unsafeFromMatrix kernel) (+) 0
    where Z :. height :. width = A.unlift $ M.shape kernel

filter :: (Elt a, IsNum a) => Exp Int -> Matrix2 a -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
filter scatter = convolve $ \point offset -> point + pure scatter * offset

-- == Morphological filters

dilate :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
dilate size = stencil (\point offset -> point + offset) size (constant 1) max (-1e20)

erode :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
erode size = stencil (\point offset -> point + offset) size (constant 1) min 1e20

opening :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
opening size = erode size . dilate size

closing :: (IsFloating a, Elt a) => Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
closing size = dilate size . erode size

