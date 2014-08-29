---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Composition.Generators.Constant where

import Flowbox.Prelude
import Flowbox.Graphics.Composition.Generators.Structures
import Math.Space.Space
import Math.Coordinate.Cartesian

import Data.Array.Accelerate as A

constant :: Grid (Exp Int) -> b -> Generator a b
constant cnv a = Generator cnv $ const a 

rectangle :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteGenerator (Exp b)
rectangle cnv@(Grid w h) interior exterior = Generator cnv $ \(Point2 x y) -> 
    A.cond (x A.<* 0 ||* y A.<* 0 ||* x A.>* w ||* y A.>* h) exterior interior
