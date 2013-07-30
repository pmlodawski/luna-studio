---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Luna.Tools.Serialization where

class Serialize a b | a -> b where
  encode :: a -> b
  decode :: b -> Either String a 
