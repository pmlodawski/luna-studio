---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Composition.Generators.Shape where

import Flowbox.Prelude
import Flowbox.Graphics.Composition.Generators.Structures
import Math.Space.Space
import Math.Coordinate.Cartesian

import Data.Array.Accelerate as A hiding (constant)

constant :: Grid (Exp Int) -> b -> Generator a b
constant cnv a = Generator cnv $ const a 

rectangle :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteGenerator (Exp b)
rectangle cnv@(Grid w h) interior exterior = bound (Constant exterior) $ constant cnv interior

ellipse :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteGenerator (Exp b)
ellipse cnv@(Grid w h) interior exterior = Generator cnv $ \(Point2 x y) -> 
    let rx = (w `div` 2); rx2 = rx * rx
        ry = (h `div` 2); ry2 = ry * ry
        x2 = (x - rx) * (x - rx)
        y2 = (y - ry) * (y - ry)
    in A.cond ((x2 * ry2 + y2 * rx2) A.<=* (rx2 * ry2)) interior exterior

bound :: Elt t => Boundary (Exp t) -> DiscreteGenerator (Exp t) -> DiscreteGenerator (Exp t)
bound b gen@(Generator cnv@(Grid width height) genF) = case b of
    Clamp      -> bnd $ \(Point2 x y) -> Point2 ((x `min` w1) `max` 0) ((y `min` h1) `max` 0) 
    Mirror     -> bnd $ \(Point2 x y) -> Point2 (abs $ -abs (x `mod` (2 * width) - w1) + w1) (abs $ -abs (y `mod` (2 * height) - h1) + h1) 
    Wrap       -> bnd $ \(Point2 x y) -> Point2 (x `mod` width) (y `mod` height) 
    Constant a -> Generator cnv $ \p@(Point2 x y) -> A.cond (x A.>=* width ||* y A.>=* height ||* x A.<* 0 ||* y A.<* 0) a (genF p)
    where bnd = flip lmap gen
          h1 = height - 1 -- FIXME [KL]: Buggy behavior with index ranges
          w1 = width - 1
