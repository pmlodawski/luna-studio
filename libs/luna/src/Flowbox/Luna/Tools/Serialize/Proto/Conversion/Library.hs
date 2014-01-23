---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library where

import           Control.Applicative
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Utils                              as AST
import           Flowbox.Luna.Data.Graph.Properties                       (Properties)
import           Flowbox.Luna.Lib.Library                                 (Library (Library))
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module     ()
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Library.Library                          as Gen
import qualified Generated.Proto.Library.Library.PropertyMap              as Gen
import qualified Generated.Proto.Library.Library.PropertyMap.KeyValue     as Gen



instance Convert (Int, Library) Gen.Library where
    encode (i, Library name path ast propertyMap) =
        Gen.Library (encodePJ i) (encodePJ name) (encodePJ path) (encodeJ ast) (encodeJ propertyMap)
    decode (Gen.Library mtid mtname mtpath mtast mtpropertyMap) = do
        i            <- decodeP <$> mtid   <?> "Failed to decode Library: 'id' field is missing"
        name         <- decodeP <$> mtname <?> "Failed to decode Library: 'name' field is missing"
        path         <- decodeP <$> mtpath <?> "Failed to decode Library: 'path' field is missing"
        tpropertyMap <- mtpropertyMap <?> "Failed to decode Library: 'propertyMap' field is missing"
        tast         <- mtast   <?> "Failed to decode Library: 'ast' field is missing"
        ast          <- decode tast
        propertyMap  <- decode tpropertyMap
        pure (i, Library name path ast propertyMap)


instance Convert (IntMap Properties) Gen.PropertyMap where
    encode pm = Gen.PropertyMap $ encodeList $ IntMap.toList pm
    decode (Gen.PropertyMap items) = IntMap.fromList <$> decodeList items


instance Convert (AST.ID, Properties) Gen.KeyValue where
    encode (i, p) = Gen.KeyValue (encodeP i) (encode p)
    decode (Gen.KeyValue ti tp) = do p <- decode tp
                                     return (decodeP ti, p)
