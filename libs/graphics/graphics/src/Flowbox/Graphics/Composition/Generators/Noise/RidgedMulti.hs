---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Composition.Generators.Noise.RidgedMulti where

import qualified Data.Array.Accelerate     as A
import           Data.Bits                 ((.&.))
import qualified Math.Coordinate.Cartesian as Cartesian

import Flowbox.Graphics.Composition.Generators.Noise.Internal
import Flowbox.Graphics.Composition.Generators.Structures     hiding (value)
import Flowbox.Prelude                                        hiding (ix)



ridgedMultiNoise :: A.Exp Double -> Generator
ridgedMultiNoise z point = ridgedMultiGen Standard 1.0 2.0 6 30 0 1.0 1.0 2.0 z point

ridgedMultiGen :: Quality -> A.Exp Double -> A.Exp Double ->
                  A.Exp Int -> A.Exp Int -> A.Exp Int ->
                  A.Exp Double -> A.Exp Double -> A.Exp Double ->
                  A.Exp Double ->
                  Generator
ridgedMultiGen quality freq lac octaveCount maxOctave seed exponent' offset gain z point _grid =
    (finalValue * 1.25) - 1.0
    where finalValue = value $
              A.iterate octaveCount octaveFunc (A.lift ((0.0 :: Double), (1.0 :: Double), point * pure freq, z*freq, (0 :: Int)))

          value args = val
              where (val, _, _, _, _) =
                        A.unlift args :: (A.Exp Double, A.Exp Double, A.Exp (Cartesian.Point2 Double), A.Exp Double, A.Exp Int)

          signal (Cartesian.Point2 sx sy) sz octv = gradientCoherentNoise quality newSeed sx sy sz
              where newSeed = (seed + octv) .&. 0x7fffffff

          octaveFunc args =
              A.lift (
                  val + signalValue * (spectralWeights A.!! curOctave)
                , newWeight A.>* 1.0 A.? (1.0, newWeight A.<* 0.0 A.? (0.0, newWeight))
                , unliftedPoint' * pure lac
                , oz * lac
                , curOctave + 1)
              where (val, curWeight, point', oz, curOctave) =
                        A.unlift args :: (A.Exp Double, A.Exp Double, A.Exp (Cartesian.Point2 Double), A.Exp Double, A.Exp Int)
                    newWeight = signalValue * gain
                    signalValue = curWeight * ((offset - abs (signal unliftedPoint' oz curOctave)) ** 2)
                    unliftedPoint' = A.unlift point'

          spectralWeights = A.generate (A.index1 maxOctave) $ \ix ->
              let A.Z A.:. i = A.unlift ix :: A.Z A.:. A.Exp Int
              in (lac ** (A.fromIntegral i)) ** (-exponent')
