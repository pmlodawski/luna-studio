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

module Flowbox.Graphics.Color.Companding.LStar where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data L = L
       deriving Show

instance (Num a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding L a where
    toLinear   _ v = v A.<=* 0.08 A.? (100 * v / k, ((v + 0.16) / 1.16) ** 3)

    fromLinear _ v = v A.<=* e A.? (v * k / 100, 1.16 * v ** (1/3) - 0.16)

k :: (A.Elt e, A.IsFloating e) => A.Exp e
k = 903.3 -- actual CIE standard
 -- 24389/27 - intent of the CIE standard

e :: (A.Elt e, A.IsFloating e) => A.Exp e
e = 0.008856 -- actual CIE standard
 -- 216/24389 - intent of the CIE standard
