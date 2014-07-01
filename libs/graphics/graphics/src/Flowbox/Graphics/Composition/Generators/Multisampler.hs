---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Flowbox.Graphics.Composition.Generators.Multisampler where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Math.Matrix                                as M

import qualified Data.Array.Accelerate                    as A
import Math.Coordinate.Cartesian                          (Point2(..))


-- FIXME: General debug required
multisampler :: Filter -> Generator -> Generator
multisampler filter generator point space = A.uncurry (/) result
    where fs = 6 -- Should be: A.floor $ 2 * window filter
          dxs = generate (A.index2 fs fs) $ \(A.unlift -> Z :. _ :. x :: EDIM2) -> A.fromIntegral $ x - (fs `div` 2)
          dys = generate (A.index2 fs fs) $ \(A.unlift -> Z :. y :. _ :: EDIM2) -> A.fromIntegral $ y - (fs `div` 2)

          start = A.lift (0.0 :: Exp Double, 0.0 :: Exp Double) :: Exp (Double, Double)
          result = sfoldl calc start A.index0 (flatten $ M.zip dxs dys)

          calc (A.unlift -> (accValues, accWeights)) (A.unlift -> (dx, dy)) = A.lift (accValues + weight * generator (point + offset) space, accWeights + weight) :: Exp (Double, Double)
              where fsd = A.fromIntegral $ fs + 2
                    offset = Point2 (dx / fsd) (dy / fsd)
                    weight = apply filter (dx / fsd) * apply filter (dy / fsd)
