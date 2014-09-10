---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Color.Companding.Gamma where

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



newtype Gamma a = Gamma a
                deriving Show

instance (Num a, Floating a) => Companding (Gamma a) a where
    toLinear   (Gamma g) v = v ** g

    fromLinear (Gamma g) v = v ** (1 / g)
