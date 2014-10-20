---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Data.Array.Accelerate    as A

#ifdef ACCELERATE_CUDA_BACKEND
import Data.Array.Accelerate.CUDA        (run)
#else
import Data.Array.Accelerate.Interpreter (run)
#endif

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.CubicBezier.Solve            as Cubic
import Flowbox.Geom2D.Accelerate.Basic
import Flowbox.Geom2D.Accelerate.CubicBezier       as CubicA
import Flowbox.Geom2D.Accelerate.CubicBezier.Solve as CubicA
import Flowbox.Math.Function.Accelerate.BSpline
import Flowbox.Prelude

foo :: Elt a => Exp (Point2 a) -> Exp (Point2 a)
foo (A.unlift -> Point2 x y) = A.lift $ Point2 x y

main :: IO ()
main = do
    putStrLn "- - - = = =   Curves Test   = = = - - -"

    return ()
