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

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                    as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian                (Point2(..), toCartesian)
import           Math.Coordinate.UV                       (toUV)


nearest :: (Elt b, IsFloating b) => Boundary (Exp b) -> Matrix2 b -> Generator (Exp b) (Exp b) 
nearest b mat = Generator $ \(Point2 x y) -> boundedIndex b mat $ A.index2 (A.truncate y) (A.truncate x)


--bilinear :: Boundary (Exp Double) -> Matrix2 Double -> Generator
--bilinear = bicubic triangle

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
