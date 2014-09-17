---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as C

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Geom2D.Accelerate.Basic
import Flowbox.Geom2D.Accelerate.CubicBezier
import Flowbox.Geom2D.Accelerate.CubicBezier.Intersection as Cubic
import Flowbox.Math.Function.Accelerate.BSpline
import Flowbox.Prelude

--changeLine :: Elt a => Line (Exp a) -> Line (Exp a)
--changeLine :: Int
--changeLine l = l
    --where Point2 a b :: Point2 a = A.unlift p1

foo :: Elt a => Exp (Point2 a) -> Exp (Point2 a)
foo (A.unlift -> Point2 x y) = A.lift $ Point2 x y

--bar :: (Elt a, IsNum a) => Exp (CubicBezier a) -> Exp (CubicBezier a)
--bar (A.unlift -> CubicBezier p1 p2 p3 p4) = A.lift $ CubicBezier p1' p2' p3' p4'
--    where p = 1
--          Point2 x1 y1 = p1
--          Point2 x2 y2 = p2
--          Point2 x3 y3 = p3
--          Point2 x4 y4 = p4
--          p1' = Point2 (x1 - 4) (y1 - 1)
--          p2' = Point2 (x2 - 3) (y2 - 2)
--          p3' = Point2 (x3 - 2) (y3 - 3)
--          p4' = Point2 (x4 - 1) (y4 - 4)

main :: IO ()
main = do
    putStrLn "- - - = = =   Bezier Test   = = = - - -"

    let p1 = Point2 0 0 :: Point2 Double
        p2 = Point2 0 1 :: Point2 Double
        p3 = Point2 1 1 :: Point2 Double
        p4 = Point2 1 0 :: Point2 Double
        curve = CubicBezier p1 p2 p3 p4
        n1 = Point2 (-0.5) 0.5
        n2 = Point2 0.5 0.5
        n3 = Point2 1.5 0.5
        o1 = Point2 (-0.5) 1.5
        o2 = Point2 0.5 (-0.5)
        o3 = Point2 2 1
        i2 = Point2 0.5 1.5
        i3 = Point2 1.5 (-0.5)
        curveT = CubicBezier n1 o1 i2 n2 :: CubicBezier Double
        node1 = BSplineNode n1 n1 o1
        node2 = BSplineNode n2 i2 o2
        node3 = BSplineNode n3 i3 o3
        spline = A.use $ fromList (Z :. 3) [node1, node2, node3] :: Acc (BSpline Double)
        solve x = A.unit $ Cubic.getValueAtX 10 0.01 (A.lift curve) x
        solveS x = A.unit $ getValueAt spline x

    print $ run $ solve 0
    print $ run $ solve 0.01
    print $ run $ solve 0.011
    print $ run $ solve 0.25
    print $ run $ solve 0.5
    print $ run $ solve 0.75
    print $ run $ solve 0.989
    print $ run $ solve 0.99
    print $ run $ solve 1
    print $ "splajny:"
    print $ run $ solveS (-1)
    print $ run $ solveS (-0.5)
    print $ run $ solveS (-0.25)
    print $ run $ solveS (0)
    print $ run $ solveS (0.25)
    print $ run $ solveS (0.5)
    print $ run $ solveS (0.75)
    print $ run $ solveS (1)
    print $ run $ solveS (1.25)
    print $ run $ solveS (1.5)
    print $ run $ solveS (1.75)

    return ()
