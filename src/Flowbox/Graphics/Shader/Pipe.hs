---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Pipe ( pipe ) where

import Flowbox.Prelude                                    as P hiding (transform)
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Matrix 
import Flowbox.Graphics.Composition.Generators.Rasterizer

import Flowbox.Math.Matrix                                as M hiding (ftrans)


pipe :: Elt a => Boundary (Exp a) 
                -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) 
                -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) 
                -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
pipe bn a b gen = fromMatrix bn $ (ftrans bn a >-> ftrans bn b) (rasterizer gen)

ftrans :: Elt a => Boundary (Exp a) -> (DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)) -> Matrix2 a -> Matrix2 a
ftrans bn f a = rasterizer $ f (fromMatrix bn a)
