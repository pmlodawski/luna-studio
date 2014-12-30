---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Composition.Generators.Matrix where

import Flowbox.Prelude
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Shape
import Flowbox.Math.Matrix                                as M

import qualified Data.Array.Accelerate                    as A
import           Math.Coordinate.Cartesian                (Point2(..))
import           Math.Space.Space


unsafeFromMatrix :: Elt e => Matrix2 e -> DiscreteGenerator (Exp e)
unsafeFromMatrix mat = Generator cnv $ \(Point2 x y) -> mat M.! A.index2 y x
    where Z :. h :. w = A.unlift $ shape mat :: EDIM2
          cnv = Grid w h 

fromMatrix :: Elt e => Boundary (Exp e) -> Matrix2 e -> DiscreteGenerator (Exp e)
fromMatrix b mat = bound b $ unsafeFromMatrix mat
