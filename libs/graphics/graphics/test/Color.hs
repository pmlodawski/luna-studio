---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Image.Color
import Flowbox.Prelude hiding (transform)

import Test.QuickCheck (quickCheck, (==>))
import Utils



input = "lena.png"
output = "out.png"

ex black ev = testFunction (exposure black ev) input output

bp black = testFunction (blackpointConvert black) input output

invertible black lift = testFunction (inverseBlackpointConvert lift . blackpointConvert black) input output
invertibleWhite white gain = testFunction (inverseWhitepointConvert gain . whitepointConvert white) input output

main = do
    --putStrLn "Testing invertibility"
    --quickCheck $ \(p :: Float) w b -> (abs (w - b) > 0.001 && w > b && p > 0.001) ==> abs (inversePointsConvert b w (pointsConvert b w p) - p) < 1.0e-10
    
    --testFunction (grade 0.08 0.85 (-0.11) 1.85 1.45 (-0.46) 0.62) input output

    ex (-0.32) 0.3