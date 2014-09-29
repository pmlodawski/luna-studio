---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Geom2D
import Geom2D.CubicBezier.Basic as Cubic
import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as C

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Geom2D.CubicBezier.Intersection as Cubic
import Flowbox.Geom2D.Accelerate.Basic
import Flowbox.Geom2D.Accelerate.CubicBezier as CubicA
import Flowbox.Geom2D.Accelerate.CubicBezier.Intersection as CubicA
import Flowbox.Math.Function.Accelerate.BSpline
import Flowbox.Prelude

--changeLine :: Elt a => Line (Exp a) -> Line (Exp a)
--changeLine :: Int
--changeLine l = l
    --where Point2 a b :: Point2 a = A.unlift p1

foo :: Elt a => Exp (Point2 a) -> Exp (Point2 a)
foo (A.unlift -> Point2 x y) = A.lift $ Point2 x y

main :: IO ()
main = do
    putStrLn "- - - = = =   Bezier Test   = = = - - -"

    let p1  = Point2 0 0 :: Point2 Double
        p2  = Point2 0 1 :: Point2 Double
        p3  = Point2 1 1 :: Point2 Double
        p4  = Point2 1 0 :: Point2 Double
        p1' = Point 0 0
        p2' = Point 0 1
        p3' = Point 1 1
        p4' = Point 1 0
        curveA = CubicA.CubicBezier p1 p2 p3 p4
        curve  = Cubic.CubicBezier p1' p2' p3' p4'
        n1 = Point2 (-0.5) 0.5
        n2 = Point2 0.5 0.5
        n3 = Point2 1.5 0.5
        o1 = Point2 (-0.5) 1.5
        o2 = Point2 0.5 (-0.5)
        o3 = Point2 2 1
        i2 = Point2 0.5 1.5
        i3 = Point2 1.5 (-0.5)
        curveT = CubicA.CubicBezier n1 o1 i2 n2 :: CubicA.CubicBezier Double
        node1  = BSplineNode n1 n1 o1
        node2  = BSplineNode n2 i2 o2
        node3  = BSplineNode n3 i3 o3
        spline = A.use $ fromList (Z :. 3) [node1, node2, node3] :: Acc (BSpline Double)
        solve  x = A.unit $ CubicA.valueAtX 10 0.01 (A.lift curveA) x
        solveS x = A.unit $ valueAt spline x
        solve' x = Cubic.valueAtX 10 0.01 curve x

    print $ "curveA:"
    print $ run $ solve 0
    print $ run $ solve 0.01
    print $ run $ solve 0.011
    print $ run $ solve 0.25
    print $ run $ solve 0.5
    print $ run $ solve 0.75
    print $ run $ solve 0.989
    print $ run $ solve 0.99
    print $ run $ solve 1
    print $ "curve:"
    print $ solve' 0
    print $ solve' 0.01
    print $ solve' 0.011
    print $ solve' 0.25
    print $ solve' 0.5
    print $ solve' 0.75
    print $ solve' 0.989
    print $ solve' 0.99
    print $ solve' 1
    --print $ "splajny:"
    --print $ run $ solveS (-1)
    --print $ run $ solveS (-0.5)
    --print $ run $ solveS (-0.25)
    --print $ run $ solveS (0)
    --print $ run $ solveS (0.25)
    --print $ run $ solveS (0.5)
    --print $ run $ solveS (0.75)
    --print $ run $ solveS (1)
    --print $ run $ solveS (1.25)
    --print $ run $ solveS (1.5)
    --print $ run $ solveS (1.75)

    return ()
