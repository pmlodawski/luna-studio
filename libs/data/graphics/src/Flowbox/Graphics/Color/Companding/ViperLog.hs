---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Color.Companding.ViperLog where

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data ViperLog = ViperLog
              deriving Show

instance (Num a, Floating a) => Companding ViperLog a where
    toLinear   _ v = 10 ** ((1023 * v - 1023) / 500)

    fromLinear _ v = (500 * logBase 10 v + 1023) / 1023
