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
import Flowbox.Prelude

--changeLine :: Elt a => Line (Exp a) -> Line (Exp a)
--changeLine :: Int
--changeLine l = l
    --where Point2 a b :: Point2 a = A.unlift p1

foo :: Elt a => Exp (Point2 a) -> Exp (Point2 a)
foo (A.unlift -> Point2 x y) = A.lift $ Point2 x y

bar :: (Elt a, IsNum a) => Exp (Line a) -> Exp (Line a)
bar (A.unlift -> Line p1 p2) = A.lift $ Line p1' p2'
    where p = 1
          Point2 x1 y1 = p1
          Point2 x2 y2 = p2
          p1' = Point2 (x1 - 4) (y1 - 3)
          p2' = Point2 (x2 - 2) (y2 - 1)

main :: IO ()
main = do
    putStrLn "- - - = = =   Bezier Test   = = = - - -"

    let p1 = Point2 1 2 :: Point2 Double
        p2 = Point2 3 4 :: Point2 Double
        l = Line p1 p2 :: Line Double
        a = A.use $ fromList (Z :. 3) [1,2,3] :: Acc (Vector Int)
        b = A.use $ fromList (Z :. 1) [l] :: Acc (Vector (Line Double))
        c = A.use $ fromList (Z :. 2) [p1, p2] :: Acc (Vector (Point2 Double))

    print $ run $ A.map bar b

    return ()
