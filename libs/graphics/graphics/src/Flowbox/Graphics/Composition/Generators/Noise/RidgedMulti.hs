---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Composition.Generators.Noise.RidgedMulti where

import qualified Data.Array.Accelerate as A
import           Data.Bits             ((.&.))

import Flowbox.Graphics.Composition.Generators.Noise.Internal
import Flowbox.Prelude                                        hiding (ix)



ridgedMultiNoise :: A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float
ridgedMultiNoise x y z = ridgedMultiGen Standard 1.0 2.0 6 30 0 1.0 1.0 2.0 x y z

ridgedMultiGen :: Quality -> A.Exp Float -> A.Exp Float ->
                  A.Exp Int -> A.Exp Int -> A.Exp Int ->
                  A.Exp Float -> A.Exp Float -> A.Exp Float ->
                  A.Exp Float -> A.Exp Float -> A.Exp Float ->
                  A.Exp Float
ridgedMultiGen quality freq lac octaveCount maxOctave seed exponent' offset gain x y z =
    (finalValue * 1.25) - 1.0
    where finalValue = value $ A.iterate octaveCount octaveFunc (A.lift ((0.0 :: Float), (1.0 :: Float), x*freq, y*freq, z*freq, (0 :: Int)))

          value args = val
              where (val, _, _, _, _, _) =
                        A.unlift args :: (A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Int)

          signal sx sy sz octv = gradientCoherentNoise quality newSeed sx sy sz
              where newSeed = (seed + octv) .&. 0x7fffffff

          octaveFunc args =
              A.lift (
                  val + signalValue * (spectralWeights A.!! curOctave)
                , newWeight A.>* 1.0 A.? (1.0, newWeight A.<* 0.0 A.? (0.0, newWeight))
                , ox * lac
                , oy * lac
                , oz * lac
                , curOctave + 1)
              where (val, curWeight, ox, oy, oz, curOctave) =
                        A.unlift args :: (A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Int)
                    newWeight = signalValue * gain
                    signalValue = curWeight * ((offset - abs (signal ox oy oz curOctave)) ** 2)

          spectralWeights = A.generate (A.index1 maxOctave) $ \ix ->
              let A.Z A.:. i = A.unlift ix :: A.Z A.:. A.Exp Int
              in (lac ** (A.fromIntegral i)) ** (-exponent')
