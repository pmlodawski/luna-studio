---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Flowbox.Tools.Conversion.Proto (
    module Flowbox.Tools.Conversion.Common,
    Convert(..)
) where

import           Flowbox.Prelude                   
import           Flowbox.Tools.Conversion.Common   



class Convert a b | a -> b, b -> a where
  encode :: a -> b
  decode :: b -> Either String a 


