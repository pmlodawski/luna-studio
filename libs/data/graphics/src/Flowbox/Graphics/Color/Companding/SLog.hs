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

module Flowbox.Graphics.Color.Companding.SLog (SLog(..)) where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data SLog = SLog
          deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding SLog a where
    toLinear   _ v = 10 ** ((v - a - b) / c) - d

    fromLinear _ v = c * logBase 10 (v + d) + b + a

a, b, c, d :: (A.Elt e, A.IsFloating e) => A.Exp e
a = 0.616596
b = 0.03
c = 0.432699
d = 0.037584
