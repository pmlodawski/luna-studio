---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Transform where

import Flowbox.Prelude hiding (transform, (.), id)

import Math.Coordinate.Cartesian (Point2(..))
import Control.Category
import Data.Monoid

import Linear.V1
import Linear.V2
import Linear.V3
import Linear.Vector (Additive(..))
import Linear.Matrix

newtype TransMatrix a = TransMatrix {
    matrix :: M33 a
} deriving (Show, Read, Eq)

instance Num a => Monoid (TransMatrix a) where
    mempty = TransMatrix $ V3 (V3 1  0  0)
                              (V3 0  1  0)
                              (V3 0  0  1)
    mappend t1 t2 = TransMatrix $ matrix t1 !*! matrix t2

transform :: Num a => TransMatrix a -> Point2 a -> Point2 a
transform mat (Point2 x y) = Point2 x' y'
    where V3 x' y' _ = matrix mat !* V3 x y 1

translate :: Num a => V2 a -> TransMatrix a
translate (V2 tx ty) = TransMatrix $ V3 (V3  1  0 (-tx))
                                        (V3  0  1 (-ty))
                                        (V3  0  0   1  )

scale :: Fractional a => V2 a -> TransMatrix a
scale (V2 sx sy) = TransMatrix $ V3 (V3 (1/sx)   0     0)
                                    (V3   0    (1/sy)  0)
                                    (V3   0      0     1)

shear :: Num a => V2 a -> TransMatrix a
shear (V2 sx sy) = TransMatrix $ V3 (V3 1  sx  0)
                                    (V3 sy  1  0)
                                    (V3 0   0  1)

rotate :: Floating a => a -> TransMatrix a
rotate phi = TransMatrix $ V3 (V3 (cos phi) ( -sin phi) 0)
                              (V3 (sin phi) (  cos phi) 0)
                              (V3 0         0           1)

