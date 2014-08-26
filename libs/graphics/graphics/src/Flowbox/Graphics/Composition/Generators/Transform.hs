---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Transform where

import Flowbox.Prelude                                    as P hiding (transform)
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Matrix
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures

import qualified Data.Array.Accelerate     as A
import           Math.Coordinate.Cartesian (Point2(..))
import           Math.Space.Space
import           Linear.V2



rotate' :: (Num a, Floating a) => a -> Point2 a -> Point2 a
rotate' phi (Point2 x y) = Point2 x' y'
    where x' = cos phi * x - sin phi * y
          y' = sin phi * x + cos phi * y

rotate :: (Num a, Floating a) => a -> CartesianGenerator a b -> CartesianGenerator a b
rotate = transform . rotate'

translate' :: Num a => V2 a -> Point2 a -> Point2 a
translate' (V2 dx dy) (Point2 x y) = Point2 (x - dx) (y - dy)

translate :: Num a => V2 a -> CartesianGenerator a b -> CartesianGenerator a b
translate = transform . translate'

scale' :: (Num a, Fractional a) => V2 a -> Point2 a -> Point2 a
scale' (V2 sx sy) (Point2 x y) = Point2 (x / sx) (y / sy)

scale :: (Num a, Fractional a) => V2 a -> CartesianGenerator a b -> CartesianGenerator a b
scale = transform . scale'

bbox :: (Ord a, Floating a) => a -> Grid a -> Grid a
bbox theta (fmap (/2) -> Grid gw gh) = Grid gw' gh'
    where pmax = Point2 (px1 `max` px2 `max` px3 `max` px4) (py1 `max` py2 `max` py3 `max` py4) 
          pmin = Point2 (px1 `min` px2 `min` px3 `min` px4) (py1 `min` py2 `min` py3 `min` py4) 
          Point2 gw' gh' = pmax - pmin
          Point2 px1 py1 = rotate' theta $ Point2 (-gw) (-gh)
          Point2 px2 py2 = rotate' theta $ Point2 ( gw) (-gh)
          Point2 px3 py3 = rotate' theta $ Point2 ( gw) ( gh)
          Point2 px4 py4 = rotate' theta $ Point2 (-gw) ( gh)

--rotateMat :: (Elt e, IsFloating e) => Exp e -> Boundary (Exp e) -> Matrix2 e -> Matrix2 e
--rotateMat phi b mat = rasterizer nsize' $ monosampler 
--                                        $ translate (V2 rx ry) 
--                                        $ rotate phi 
--                                        $ translate (V2 tx ty) 
--                                        $ interpolator triangle 
--                                        $ fromMatrix b $ mat
--    where A.Z A.:. height A.:. width = A.unlift $ M.shape mat
--          size = fmap A.fromIntegral $ Grid width height
--          Grid tx ty = fmap (/ (-2)) size
--          nsize = bbox phi size
--          nsize' = fmap A.ceiling nsize
--          Grid rx ry = fmap (/ (2)) nsize
