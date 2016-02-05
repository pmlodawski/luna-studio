---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Flowbox.Graphics.Shader.Pipe ( pipe ) where

import           Flowbox.Graphics.Shader.Matrix
import           Flowbox.Graphics.Shader.Rasterizer
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Prelude                    as P hiding (transform)

import           Flowbox.Math.Matrix                as M hiding (ftrans)


pipe :: Elt a
     => Boundary (Exp a)
     -> (DiscreteShader (Exp a) -> DiscreteShader (Exp a))
     -> (DiscreteShader (Exp a) -> DiscreteShader (Exp a))
     -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
pipe bn a b gen = fromMatrix bn $ (ftrans bn a >-> ftrans bn b) (rasterizer gen)

ftrans :: Elt a => Boundary (Exp a) -> (DiscreteShader (Exp a) -> DiscreteShader (Exp a)) -> Matrix2 a -> Matrix2 a
ftrans bn f a = rasterizer $ f (fromMatrix bn a)
