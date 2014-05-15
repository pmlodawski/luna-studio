---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Bus.Data.Exception where

import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Bus.Exception                  as Gen



data Exception = Exception { msg :: Maybe String }


instance ConvertPure Exception Gen.Exception where
    encodeP (Exception msg')     = Gen.Exception $ fmap encodeP msg'
    decodeP (Gen.Exception msg') = Exception     $ fmap decodeP msg'

