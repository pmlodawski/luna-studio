---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Color.Companding.SRGB where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data SRGB = SRGB
          deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsScalar t) => Companding SRGB a where
    toLinear   _ v = v A.<=* 0.04045 A.? (v / 12.92, ((v + 0.055) / 1.055) ** 2.4)
    
    fromLinear _ v = v A.<=* 0.0031308 A.? (12.92 * v, (1.055 * v) ** (1 / 2.4) - 0.055)
