---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Flowbox.Graphics.Composition.Generator.Noise.Billow where

import qualified Data.Array.Accelerate     as A
import           Data.Bits                 ((.&.))
import qualified Math.Coordinate.Cartesian as Cartesian

import Flowbox.Graphics.Composition.Generator.Noise.Internal
import Flowbox.Graphics.Shader.Shader
import Flowbox.Prelude



billowNoise :: (A.Elt a, A.IsFloating a, a ~ A.Plain a, A.Lift A.Exp a) => A.Exp a -> CartesianShader (A.Exp a) (A.Exp a)
billowNoise z = billowGen Standard 1.0 2.0 6 0.5 0 z

billowGen :: forall a. (A.Elt a, A.IsFloating a, a ~ A.Plain a, A.Lift A.Exp a) =>
             Quality -> A.Exp a -> A.Exp a ->
             A.Exp Int -> A.Exp a -> A.Exp Int ->
             A.Exp a ->
             CartesianShader (A.Exp a) (A.Exp a)
billowGen quality freq lac octaveCount persistence seed z = unitShader $ \point ->
    0.5 + value (A.iterate octaveCount octaveFunc (A.lift (0.0 :: a, 1.0 :: a, point * pure freq, z*freq, 0 :: Int)))
    where value args = val
              where (val, _, _, _, _) =
                        A.unlift args :: (A.Exp a, A.Exp a, A.Exp (Cartesian.Point2 a), A.Exp a, A.Exp Int)

          signal (Cartesian.Point2 sx sy) sz octv =
              2.0 * abs (gradientCoherentNoise quality ((seed + octv) .&. 0xffffffff) sx sy sz) - 1.0

          octaveFunc args =
              A.lift (
                  val + (signal unliftedPoint' oz curOctave * curPersistence)
                , curPersistence * persistence
                , unliftedPoint' * pure freq
                , oz * lac
                , curOctave + 1)
              where (val, curPersistence, point', oz, curOctave) =
                        A.unlift args :: (A.Exp a, A.Exp a, A.Exp (Cartesian.Point2 a), A.Exp a, A.Exp Int)
                    unliftedPoint' = A.unlift point'
