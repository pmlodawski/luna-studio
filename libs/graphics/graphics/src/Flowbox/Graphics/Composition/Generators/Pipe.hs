---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Pipe ( pipe ) where

import Flowbox.Prelude                                    as P hiding (transform)
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Rasterizer

import Flowbox.Math.Matrix                                as M hiding (ftrans)
import Math.Space.Space


pipe :: Elt a => Grid (Exp Int) -> Boundary (Exp a) 
                -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) 
                -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) 
                -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
pipe g bn a b gen = fromMatrix bn $ (ftrans g bn a >-> ftrans g bn b) (rasterizer g gen)

ftrans :: Elt a => Grid (Exp Int) -> Boundary (Exp a) -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) -> Matrix2 a -> Matrix2 a
ftrans g bn f a = rasterizer g $ f (fromMatrix bn a)
