---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Sampler where

import Flowbox.Prelude                                    as P hiding (transform)
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Convolution
import Flowbox.Math.Matrix                                as M

import qualified Data.Array.Accelerate                    as A
import           Math.Coordinate.Cartesian                (Point2(..))
import           Linear.V2



unsafeFromMatrix :: Elt e => Matrix2 e -> DiscreteGenerator (Exp e)
unsafeFromMatrix mat = Generator $ \(Point2 x y) -> mat M.! A.index2 y x

fromMatrix :: Elt e => Boundary (Exp e) -> Matrix2 e -> DiscreteGenerator (Exp e)
fromMatrix b mat = Generator $ \(Point2 x y) -> boundedIndex b mat $ A.index2 y x

monosampler :: (Elt a, IsNum a) => Generator (Exp a) e -> DiscreteGenerator e
monosampler = transform $ fmap A.fromIntegral

multisampler :: (Elt e, IsNum e, IsFloating e) => Matrix2 e -> Generator (Exp e) (Exp e) -> DiscreteGenerator (Exp e)
multisampler kernel = convolve msampler kernel
    where fi = fmap A.fromIntegral
          msampler point offset = fi point + subpixel * fi offset 
          Z :. h :. w = A.unlift $ shape kernel
          subpixel = Point2 (1 / A.fromIntegral (w - 1)) (1 / A.fromIntegral (h - 1))

interpolator :: (Elt e, IsNum e, IsFloating e) => Matrix2 e -> DiscreteGenerator (Exp e) -> Generator (Exp e) (Exp e)
interpolator filter (Generator gen) = Generator $ \(Point2 x y) ->
    let ksh = shape filter
        Z :. h :. w = A.unlift ksh
        dxs = generate ksh $ \(A.unlift -> Z :. y :. x :: EDIM2) -> A.fromIntegral (x - w `div` 2)
        dys = generate ksh $ \(A.unlift -> Z :. y :. x :: EDIM2) -> A.fromIntegral (y - h `div` 2)
        offsets = flatten $ M.zip3 filter dxs dys

        calc (valSum, weightSum) (weight, dx, dy) = (valSum + value * weight, weightSum + weight)
            where value = gen $ Point2 (A.floor $ x + dx) (A.floor $ y + dy)
    in A.uncurry (/) $ sfoldl (A.lift2 calc) (A.constant (0, 0)) A.index0 offsets

nearest :: (Elt e, IsFloating e) => Boundary (Exp e) -> Matrix2 e -> ContinousGenerator (Exp e)
nearest b mat = Generator $ \(Point2 x y) -> boundedIndex b mat $ A.index2 (A.truncate y) (A.truncate x)

-- TODO [KL]: Rewrite
--bicubic :: (Elt a, IsNum a, IsFloating a) => Filter a -> Boundary (Exp a) -> Matrix2 a -> Generator (Exp a) (Exp a)
--bicubic filter b mat = Generator $ \pixel newSpace ->
--    let Z :. oldHeight :. oldWidth = A.unlift $ shape mat :: EDIM2
--        oldSpace = Grid (A.fromIntegral oldWidth) (A.fromIntegral oldHeight)
--        Point2 x' y' = toCartesian oldSpace $ toUV newSpace pixel

--        fs = 6 -- Should be: A.floor $ 2 * window filter -- TODO: conditional based on the scale ratio
--        offsets = fromList (Z :. (fs * fs)) [(x - (fs `div` 2), y - (fs `div` 2)) | x <- [0..fs], y <- [0..fs]]

--        lift :: Elt a => Exp a -> Exp (a, a)
--        lift a = A.lift (a, a)

--        result = sfoldl calc (lift 0) A.index0 offsets

--        xs  = let xs' = width newSpace / width oldSpace   in A.cond (xs' A.<* 1) xs' 1
--        ys  = let ys' = height newSpace / height oldSpace in A.cond (ys' A.<* 1) ys' 1

--        calc (A.unlift -> (valSum, weightSum)) (A.unlift -> (dx, dy)) = A.lift (valSum + value * weight, weightSum + weight)
--            where value  = boundedIndex b mat $ A.index2 (A.floor y' + dy) (A.floor x' + dx)
--                  wx = apply filter $ (A.fromIntegral dx - frac x') * xs
--                  wy = apply filter $ (A.fromIntegral dy - frac y') * ys
--                  weight = wx * wy
--    in A.uncurry (/) result
