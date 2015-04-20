---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Convertible.Instances.Missing where

import Data.ByteString
import Data.Convertible
import Prelude



instance Convertible a b => Convertible [a] [b] where
    safeConvert = sequence . fmap safeConvert
