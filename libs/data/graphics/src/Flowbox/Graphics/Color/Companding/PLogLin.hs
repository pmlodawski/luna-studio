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

module Flowbox.Graphics.Color.Companding.PLogLin where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude



data PLogLin = PLogLin
             deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding PLogLin a where
    toLinear   _ v = 10 ** ((v * 1023 - logReference) * dpcvOverNg) * linReference

    fromLinear _ v = ((logBase 10 (linReference * v) / dpcvOverNg) + logReference) / 1023

logReference,  linReference, dpcvOverNg, densityPerCodeValue, negativeGamma :: (A.Elt e, A.IsFloating e) => A.Exp e
logReference = 445
linReference = 0.18
dpcvOverNg   = densityPerCodeValue / negativeGamma
densityPerCodeValue = 0.002
negativeGamma = 0.6
