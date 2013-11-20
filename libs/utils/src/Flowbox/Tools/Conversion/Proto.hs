---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE MultiParamTypeClasses  #-}

module Flowbox.Tools.Conversion.Proto (
    Convert(..),
    ConvertPure(..),
) where

import           Flowbox.Prelude   



class Convert a b | a -> b where
  encode :: a -> b
  decode :: b -> Either String a 


class ConvertPure a b | a -> b where
  encodeP :: a -> b
  decodeP :: b -> a 