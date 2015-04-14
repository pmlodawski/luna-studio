---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Bus.Data.Serialize.Proto.Conversion.Message where

import qualified Data.ByteString.Lazy        as ByteString

import           Flowbox.Bus.Data.Message    as M
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import           Generated.Proto.Bus.Message as Gen

instance ConvertPure M.Message Gen.Message where
    encodeP (M.Message topic message) = Gen.Message (encodeP topic) (ByteString.fromStrict message)
    decodeP (Gen.Message topic message) = M.Message (decodeP topic) (ByteString.toStrict   message)

instance ConvertPure M.RequestID M.RequestID where
    encodeP = id
    decodeP = id
