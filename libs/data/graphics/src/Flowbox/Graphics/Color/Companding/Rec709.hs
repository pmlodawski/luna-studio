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

module Flowbox.Graphics.Color.Companding.Rec709 where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data Rec709 = Rec709
            deriving Show

instance (Num a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Rec709 a where
    toLinear   _ v = v A.<=* 0.081 A.? (v / 4.5, ((v + 0.099) / 1.099) ** (1 / 0.45))

    fromLinear _ v = v A.<=* 0.018 A.? (v * 4.5, (1.099 * v ** 0.45) - 0.099)
