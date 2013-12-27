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
import qualified Generated.Proto.Nodedefault.DefaultsMap.Entry as Gen
import qualified Flowbox.Luna.Data.Graph.Node          as Node



instance Convert DefaultsMap Gen.DefaultsMap where
    encode = Gen.DefaultsMap . encodeList . Map.toList
    decode (Gen.DefaultsMap items) = Map.fromList <$> decodeList items


instance Convert (InPort, (Node.ID, Value)) Gen.Entry where
    encode (inPort, (nodeID, value)) = Gen.Entry (encodePJ inPort) (encodePJ nodeID) (encodePJ value)
    decode (Gen.Entry minPort mnodeID mvalue) = do
        inPort <- minPort <?> "Failed to decode DefaultsMap.Entry: 'inPort' field is missing"
        nodeID <- mnodeID <?> "Failed to decode DefaultsMap.Entry: 'nodeID' field is missing"
        value  <- mvalue  <?> "Failed to decode DefaultsMap.Entry: 'value' field is missing"
        return (decodeP inPort, (decodeP nodeID, decodeP value))

