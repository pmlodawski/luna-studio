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

module Flowbox.Graphics.Color.Companding.AlexaV3LogC (AlexaV3LogC(..)) where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data AlexaV3LogC = AlexaV3LogC
                 deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding AlexaV3LogC a where
    toLinear   _ v = v A.>* eCutF A.? ((10 ** ((v - d) / c) - b) / a, (v - f) / e)

    fromLinear _ v = v A.>* cut A.? (c * logBase 10 (a * v + b) + d, e * v + f)

a, b, c, d, e, f, cut, eCutF :: (A.Elt a, A.IsFloating a) => A.Exp a
a     = 5.555556
b     = 0.052272
c     = 0.247190
d     = 0.385537
e     = 5.367655
f     = 0.092809
cut   = 0.010591
eCutF = e * cut + f
