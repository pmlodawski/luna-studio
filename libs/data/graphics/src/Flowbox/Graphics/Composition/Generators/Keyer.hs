---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Composition.Generators.Keyer where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Prelude



type KeyerLine a = A.Exp (a, a, a, a)

keyer :: (A.Elt a, A.IsFloating a) => KeyerLine a -> Generator x (A.Exp a) -> Generator x (A.Exp a)
keyer (A.unlift -> (a, b, c, d)) gen = flip fmap gen $ \pixelValue ->
    let alphaValue = A.cond (pixelValue A.<=* a) 0
                   $ A.cond (pixelValue A.<=* b) (risingEdgePosition pixelValue)
                   $ A.cond (pixelValue A.<=* c) 1
                   $ A.cond (pixelValue A.<=* d) (fallingEdgePosition pixelValue)
                   $ 0

        -- simply linear functions
        risingEdgePosition x  = A.cond (a A.==* b) 1
                              $ A.cond (a A.==* 0) 0
                              $ (1 / (b - a)) * x + (b / a)
        fallingEdgePosition x = A.cond (c A.==* d) 1
                              $ A.cond (d A.==* 0) 0
                              $ (1 / (c - d)) * x + (c / d)
    in alphaValue
