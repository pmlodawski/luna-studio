---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Convertible.Instances.Missing where

import           Data.ByteString
import           Data.Convertible
import           Prelude
--import Data.Traversable (Traversable, sequenceA)



instance Convertible a b => Convertible [a] [b] where
    safeConvert = sequence . fmap safeConvert

--FIXME[WD]: make this work!
--instance (Functor f, Traversable f, Convertible a b) => Convertible (f a) (f b) where
--    safeConvert = sequenceA . fmap safeConvert
