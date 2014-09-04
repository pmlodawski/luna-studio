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
import Flowbox.Geom2D.Accelerate.CubicBezier.Intersection
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
        --l = Line p1 p2 :: Line Double
        curve = CubicBezier p1 p2 p3 p4
        --a = A.use $ fromList (Z :. 3) [1,2,3] :: Acc (Vector Int)
        --b = A.use $ fromList (Z :. 1) [l] :: Acc (Vector (Line Double))
        --c = A.use $ fromList (Z :. 2) [p1, p2] :: Acc (Vector (Point2 Double))
        curves = A.use $ fromList (Z :. 1) [curve] :: Acc (Vector (CubicBezier Double))
        root = A.unit $ findCubicYforX 10 0.01 (A.lift curve) 0.25

    print $ run $ root

    return ()
