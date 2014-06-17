---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generators.Noise.Perlin where

import qualified Data.Array.Accelerate as A
import           Data.Bits             ((.&.))

import Flowbox.Graphics.Composition.Generators.Noise.Internal
import Flowbox.Prelude



perlinNoise :: A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float
perlinNoise x y z = perlinGen Standard 1.0 2.0 6 0.5 0 x y z

perlinGen :: Quality -> A.Exp Float -> A.Exp Float ->
             A.Exp Int -> A.Exp Float -> A.Exp Int ->
             A.Exp Float -> A.Exp Float -> A.Exp Float ->
             A.Exp Float
perlinGen quality freq lac octaveCount persistence seed x y z =
    value $ A.iterate octaveCount octaveFunc (A.lift ((0.0 :: Float), (1.0 :: Float), x*freq, y*freq, z*freq, (0 :: Int)))
    where value args = val
              where (val, _, _, _, _, _) =
                        A.unlift args :: (A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Int)

          signal sx sy sz octv = gradientCoherentNoise quality ((seed + octv) .&. 0xffffffff) sx sy sz
    
          octaveFunc args =
              A.lift (
                  val + (signal ox oy oz (curOctave) * curPersistence)
                , curPersistence * persistence
                , ox * lac
                , oy * lac
                , oz * lac
                , curOctave + 1)
              where (val, curPersistence, ox, oy, oz, curOctave) =
                        A.unlift args :: (A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Float, A.Exp Int)
