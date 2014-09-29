---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Color.Companding where

import Flowbox.Prelude



class (Num b, Floating b) => Companding a b where
    toLinear   :: a -> b -> b
    fromLinear :: a -> b -> b
