---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Bus.Data.Exception where

import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Bus.Exception as Gen



data Exception = Exception { _msg :: Maybe String }


makeLenses(''Exception)

instance ConvertPure Exception Gen.Exception where
    encodeP (Exception msg')     = Gen.Exception $ fmap encodeP msg'
    decodeP (Gen.Exception msg') = Exception     $ fmap decodeP msg'

