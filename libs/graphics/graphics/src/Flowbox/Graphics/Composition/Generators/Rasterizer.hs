---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Flowbox.Graphics.Composition.Generators.Rasterizer where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Transform
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian (Point2(..))
import           Data.Monoid



rasterizer :: Elt e => DiscreteGenerator (Exp e) -> Matrix2 e
rasterizer (Generator (Grid width height) gen) = generate (A.index2 height width) wrapper
    where wrapper (A.unlift -> Z :. y :. x :: EDIM2) = gen (Point2 x y)

gridRasterizer :: forall a e . (Elt e, Elt a, IsFloating a)
               => Grid (Exp Int) -> Grid (Exp Int) 
               -> (CartesianGenerator (Exp a) (Exp e) -> DiscreteGenerator (Exp e))
               -> [CartesianGenerator (Exp a) (Exp e)] -> Matrix2 e
gridRasterizer space grid sampler generators = generate (A.index2 (height space) (width space)) wrapper
    where cell = div <$> space <*> grid
          rasterized = mconcat $ P.map (flatten . rasterizer . sampler . scaleTo cell) generators :: Vector e
          wrapper (A.unlift -> Z :. y :. x :: EDIM2) = rasterized M.!! (i `mod` size rasterized)
              where i = (cw * ch) * gridi + celli
                    Grid cw ch = cell
                    gridi = x `div` cw + (height grid - y `div` ch - 1) * width grid
                    celli = x `mod` cw + (y `mod` ch) * cw
