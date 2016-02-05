---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- TODO: change this module name to something more apropriate, like Query?
module Flowbox.Geom2D.Accelerate.CubicBezier.Solve where

import           Data.Array.Accelerate                 as A

import           Flowbox.Geom2D.Accelerate.CubicBezier ()
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Graphics.Utils.Utils          (fstTrio, sndTrio, trdTrio)
import           Flowbox.Prelude                       hiding (fst, lift, snd, (<*), (?))
import           Math.Coordinate.Cartesian             (Point2 (..))



valueAtX :: forall a b. (Elt a, IsFloating a, Integral b) => Exp Int -> Exp a -> Exp (CubicBezier a) -> Exp a -> Exp a
valueAtX limit eps (unlift -> curve) x = solvey
    $ cond (x <=* x1 ||* err x1 <=* eps) 0
        $ cond (x >=* x4 ||* err x4 <=* eps) 1
            $ mid $ sndTrio $ while (\v -> fstTrio v <* limit &&* err (trdTrio v) >* eps) (lift1 step) $ lift (0 :: Exp Int, startAt, solvex $ mid startAt)
    where CubicBezier (Point2 x1 y1) (Point2 x2 y2) (Point2 x3 y3) (Point2 x4 y4) = curve
          step :: (Exp Int, Exp (a, a), Exp a) -> (Exp Int, Exp (a, a), Exp a)
          step (s, t@(unlift -> (a::Exp a, b::Exp a)), x') = let
                                                    m = mid t :: Exp a
                                                    t' = x <* x' ? (lift (a, m), lift (m, b))
                                                in (s+1, t', solvex $ mid t')
          mid (A.unlift -> (a, b)) = (a + b) / 2
          err x'   = abs $ x - x'
          startAt  = A.constant (0, 1)
          solvex t = (1-t)^(3::b) * x1 + 3*(1-t)^(2::b)*t * x2 + 3*(1-t)*t^(2::b) * x3 + t^(3::b) * x4
          solvey t = (1-t)^(3::b) * y1 + 3*(1-t)^(2::b)*t * y2 + 3*(1-t)*t^(2::b) * y3 + t^(3::b) * y4
