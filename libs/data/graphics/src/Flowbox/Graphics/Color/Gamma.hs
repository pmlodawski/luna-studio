---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.Graphics.Color.Gamma where

import Flowbox.Prelude



data GammaCorrection = Linear | GammaCorrected

class GammaCorrectible a c where
    type GammaT (a :: GammaCorrection -> * -> *) c :: *
    toLinear   :: a 'GammaCorrected c -> a 'Linear c
    fromLinear :: a 'Linear c -> a 'GammaCorrected c
    gamma      :: a b c -> GammaT a c
