---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import           Data.Array.Accelerate                       as A hiding (length)
import           Data.Array.Accelerate.CUDA                  as C

import           Flowbox.Geom2D.Accelerate.CubicBezier.Solve
import           Flowbox.Geom2D.CubicBezier
import           Math.Coordinate.Cartesian                   (Point2 (..))

import           Flowbox.Prelude



main :: IO ()
main = do
    print "- - - = = =   Func Test  = = = - - -"

    let curve = A.lift $ CubicBezier (Point2 (0::Double) 1) (Point2 0.03 1) (Point2 0.06 0) (Point2 0.09 0)
        values = fmap (/100) [0..9] :: [Double]
        results = A.map (valueAtX 10 0.001 curve) $ A.use $ A.fromList (Z:.length values) values

    print $ C.run results

    --let func = bsplineLinearFromPoints
