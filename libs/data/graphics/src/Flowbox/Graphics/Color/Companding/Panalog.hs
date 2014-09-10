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

module Flowbox.Graphics.Color.Companding.Panalog (Panalog(..)) where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data Panalog = Panalog
             deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Panalog a where
    toLinear   _ v = (10 ** ((1023 * v - 681) / 444) - panalogBlackOffset) / (1 - panalogBlackOffset)

    fromLinear _ v = (681 + 444 * logBase 10 (v * (1 - panalogBlackOffset) + panalogBlackOffset)) / 1023

panalogBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
panalogBlackOffset = 10 ** ((64 - 681) / 444)
