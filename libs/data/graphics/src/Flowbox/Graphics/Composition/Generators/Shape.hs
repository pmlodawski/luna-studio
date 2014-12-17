---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generators.Shape where


import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Prelude    hiding (transform)
import Flowbox.Math.Index hiding (canvas)

import Math.Space.Space
import Math.Coordinate.Cartesian hiding (x, y, w)
import Data.Array.Accelerate     as A hiding (constant)



constant :: Grid (Exp Int) -> b -> Generator a b
constant cnv a = Generator cnv $ const a 

rectangle :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteGenerator (Exp b)
rectangle cnv interior exterior = bound (Constant exterior) $ constant cnv interior

ellipse :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteGenerator (Exp b)
ellipse cnv@(Grid w h) interior exterior = Generator cnv $ \(Point2 x y) -> 
    let rx = (w `div` 2)
    	rx2 = rx * rx
        ry = (h `div` 2)
        ry2 = ry * ry
        x2 = (x - rx) * (x - rx)
        y2 = (y - ry) * (y - ry)
    in A.cond ((x2 * ry2 + y2 * rx2) A.<=* (rx2 * ry2)) interior exterior

bound :: Elt t => Boundary (Exp t) -> DiscreteGenerator (Exp t) -> DiscreteGenerator (Exp t)
bound b gen = Generator (canvas gen) (boundedIndex2D b gen)
