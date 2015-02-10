---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.Graphics.Color.Companding.Gamma where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Graphics.Utils.Utils      (variable)
import Flowbox.Prelude



data Gamma a = Gamma a
			 deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.IsFloating t, A.Elt t) => Companding (Gamma a) (A.Exp t) where
    toLinear   (Gamma g) v = v ** (variable g)

    fromLinear (Gamma g) v = v ** (1 / (variable g))
