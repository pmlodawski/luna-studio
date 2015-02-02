---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Shader.Rasterizer where

--import Flowbox.Graphics.Composition.Transform
import Flowbox.Graphics.Shader.Shader
import Flowbox.Math.Matrix                    as M
import Flowbox.Prelude                        as P

import qualified Data.Array.Accelerate     as A
import           Data.Monoid
import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Space.Space



rasterizer :: Elt e => DiscreteShader (Exp e) -> Matrix2 e
rasterizer (Shader (Grid width' height') gen) = generate (A.index2 height' width') wrapper
    where wrapper (A.unlift . A.unindex2 -> (y, x)) = gen (Point2 x y)

--TODO[KM]: figure out if this should be placed here (first of all, it probably should go into compo, second of all the use of scale causes an import cycle)
--gridRasterizer :: forall a e . (Elt e, Elt a, IsFloating a)
--               => Grid (Exp Int) -> Grid (Exp Int)
--               -> (CartesianShader (Exp a) (Exp e) -> DiscreteShader (Exp e))
--               -> [CartesianShader (Exp a) (Exp e)] -> Matrix2 e
--gridRasterizer space grid sampler generators = generate (A.index2 (height space) (width space)) wrapper
--    where cell = div <$> space <*> grid
--          rasterized = mconcat $ P.map (flatten . rasterizer . sampler . scale cell) generators :: Vector e
--          wrapper (A.unlift . A.unindex2 -> (y, x)) = rasterized M.!! (i `mod` size rasterized)
--              where i = (cw * ch) * gridi + celli
--                    Grid cw ch = cell
--                    gridi = x `div` cw + (height grid - y `div` ch - 1) * width grid
--                    celli = x `mod` cw + (y `mod` ch) * cw
