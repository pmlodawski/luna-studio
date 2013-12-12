---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.NodeDefault where

import qualified Data.Map as Map

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.Graph.Default.DefaultsMap      (DefaultsMap)
import           Flowbox.Luna.Data.Graph.Default.Value            (Value)
import           Flowbox.Luna.Data.Graph.Port                     (InPort)
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Nodedefault.DefaultsMap          as Gen
import qualified Generated.Proto.Nodedefault.DefaultsMap.KeyValue as Gen



instance Convert DefaultsMap Gen.DefaultsMap where
    encode = Gen.DefaultsMap . encodeList . Map.toList
    decode (Gen.DefaultsMap items) = Map.fromList <$> decodeList items


instance Convert (InPort, Value) Gen.KeyValue where
    encode (inPort, value) = Gen.KeyValue (encodePJ inPort) (encodePJ value)
    decode (Gen.KeyValue minPort mvalue) = do
        inPort <- minPort<?> "Failed to decode Properties: 'inPort' field is missing"
        value  <- mvalue <?> "Failed to decode Properties: 'value' field is missing"
        return (decodeP inPort, decodeP value)

