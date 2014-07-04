---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Rasterizer where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian (Point2(..))
import           Data.Monoid


rasterizer :: Grid (Exp Int) -> Generator -> Matrix2 Double
rasterizer space (Generator gen) = generate (A.index2 (height space) (width space)) wrapper
    where wrapper (A.unlift -> Z :. y :. x :: EDIM2) = let pixel = Point2 (A.fromIntegral x) (A.fromIntegral y)
                                                       in gen pixel $ A.fromIntegral <$> space

gridRasterizer :: Grid (Exp Int) -> Grid (Exp Int) -> [Generator] -> Matrix2 Double
gridRasterizer space grid generators = generate (A.index2 (height space) (width space)) wrapper
    where cell = div <$> space <*> grid
          rasterized = mconcat $ P.map (flatten . (rasterizer cell)) generators :: Vector Double
          wrapper (A.unlift -> Z :. y :. x :: EDIM2) = rasterized M.!! (i `mod` size rasterized)
              where i = (cw * ch) * gridi + celli
                    Grid cw ch = cell
                    gridi = x `div` cw + (height grid - y `div` ch - 1) * width grid
                    celli = x `mod` cw + (y `mod` ch) * cw
