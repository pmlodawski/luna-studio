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

import Flowbox.Prelude        as P hiding ((<*))
import Flowbox.Math.Matrix    as M
import Data.Array.Accelerate  as A

import Math.Space.Space

data Filter a = Filter { window :: Exp a
                       , apply :: Exp a -> Exp a
                       }

box :: (Elt a, IsFloating a) => Filter a
box = Filter 0.5 $ \t -> A.cond (t >* -0.5 &&* t <=*  0.5) 1.0 0.0

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
