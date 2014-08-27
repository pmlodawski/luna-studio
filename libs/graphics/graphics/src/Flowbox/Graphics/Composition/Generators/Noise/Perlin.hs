---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generators.Noise.Perlin where

import qualified Data.Array.Accelerate      as A
import           Data.Bits                  ((.&.))
import qualified Math.Coordinate.Cartesian  as Cartesian
import           Math.Space.Space          (Grid)

import Flowbox.Graphics.Composition.Generators.Noise.Internal
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Prelude



perlinNoise :: A.Exp Double -> ContinousGenerator (A.Exp Double)
perlinNoise z = unitGenerator $ runGenerator $ perlinGen Standard 1.0 2.0 6 0.5 0 z

perlinGen :: Quality -> A.Exp Double -> A.Exp Double ->
             A.Exp Int -> A.Exp Double -> A.Exp Int ->
             A.Exp Double ->
             ContinousGenerator (A.Exp Double)
perlinGen quality freq lac octaveCount persistence seed z = unitGenerator $ \point ->
    value $ A.iterate octaveCount octaveFunc (A.lift (0.0 :: Double, 1.0 :: Double, point * pure freq, z*freq, 0 :: Int))
    where value args = val
              where (val, _, _, _, _) =
                        A.unlift args :: (A.Exp Double, A.Exp Double, A.Exp (Cartesian.Point2 Double), A.Exp Double, A.Exp Int)

          signal (Cartesian.Point2 sx sy) sz octv = gradientCoherentNoise quality newSeed sx sy sz
              where newSeed = (seed + octv) .&. 0xffffffff

          octaveFunc args =
              A.lift (
                  val + signal unliftedPoint' oz curOctave * curPersistence
                , curPersistence * persistence
                , unliftedPoint' * pure lac
                , oz * lac
                , curOctave + 1)
              where (val, curPersistence, point', oz, curOctave) =
                        A.unlift args :: (A.Exp Double, A.Exp Double, A.Exp (Cartesian.Point2 Double), A.Exp Double, A.Exp Int)
                    unliftedPoint' = A.unlift point' :: Cartesian.Point2 (A.Exp Double)
