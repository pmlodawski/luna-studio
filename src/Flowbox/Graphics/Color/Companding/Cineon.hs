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

module Flowbox.Graphics.Color.Companding.Cineon (Cineon(..)) where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data Cineon = Cineon
            deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Cineon a where
    toLinear   _ v = (10 ** ((1023 * v - 685) / 300) - cineonBlackOffset) / (1 - cineonBlackOffset)

    fromLinear _ v = (685 + 300 * logBase 10 (v * (1 - cineonBlackOffset) + cineonBlackOffset)) / 1023

cineonBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
cineonBlackOffset = 10 ** ((95 - 685) / 300)
