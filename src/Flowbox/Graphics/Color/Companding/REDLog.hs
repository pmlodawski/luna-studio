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

module Flowbox.Graphics.Color.Companding.REDLog where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data REDLog = REDLog
            deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding REDLog a where
    toLinear   _ v = ((10 ** (1023 * v - 1023) / 511) - redBlackOffset) / (1 - redBlackOffset)

    fromLinear _ v = (1023 + 511 * logBase 10 (v * (1 - redBlackOffset) + redBlackOffset)) / 1023

redBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
redBlackOffset = 10 ** ((0 - 1023) / 511)
