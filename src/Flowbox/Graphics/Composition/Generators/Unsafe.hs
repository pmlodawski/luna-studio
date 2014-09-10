---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Flowbox.Graphics.Composition.Generators.Unsafe where

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.IO          as A
import qualified Data.Vector.Storable              as SV
import qualified Data.Vector.Storable.Mutable      as SV
import qualified Math.Coordinate.Cartesian         as Cartesian
import           Math.Space.Space                  (Grid(..))

import Flowbox.Prelude



data MImage a = MImage { vector :: SV.IOVector a
                       , canvas :: A.DIM2
                       }

index :: (SV.Storable a) => A.Boundary a -> MImage a -> Cartesian.Point2 Int -> IO a
index boundary mimage@MImage{..} (Cartesian.Point2 x y) =
    case boundary of
        A.Clamp      -> mimage `unsafeShapeIndex` (A.Z A.:. ((x `min` width) `max` 0) A.:. ((y `min` height) `max` 0))
        A.Mirror     -> mimage `unsafeShapeIndex` (A.Z A.:. (abs $ -abs (x `mod` (2 * width) - width) + width) A.:. (abs $ -abs (y `mod` (2 * height) - height) + height))
        A.Wrap       -> mimage `unsafeShapeIndex` (A.Z A.:. (x `mod` width) A.:. (y `mod` height))
        A.Constant a -> if (x >= width || y >= height || x < 0 || y < 0)
                        then return a
                        else mimage `unsafeShapeIndex` (A.Z A.:. x A.:. y)
    where A.Z A.:. height A.:. width = canvas

unsafeShapeIndex :: (SV.Storable a) => MImage a -> A.DIM2 -> IO a
unsafeShapeIndex MImage{..} sh = vector `SV.unsafeRead` (Sugar.toIndex canvas sh)
