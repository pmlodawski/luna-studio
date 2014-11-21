---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Data.Serialize.Proto.Conversion.Name where

import qualified Data.Map as Map

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Name.Name                      as Gen
import           Luna.AST.Name                                  (Name (Name))
import qualified Luna.AST.Name                                  as Name
import qualified    Generated.Proto.Name.Segment as Gen
import qualified Generated.Proto.Name.Segment.Cls as Gen


instance Convert Name Gen.Name where
    encode (Name base segments) = Gen.Name (encodePJ base) (encodeList segments)
    decode (Gen.Name base segments) =
        Name <$> decodePJ base (missing "Name" "base")
             <*> decodeList segments


instance Convert Name.Segment Gen.Segment where
    encode (Name.Token str) = Gen.Segment Gen.Token $ encodePJ str
    encode  Name.Hole       = Gen.Segment Gen.Hole    Nothing
    decode (Gen.Segment Gen.Token name) = Name.Token <$> decodePJ name (missing "Name.Segment" "name")
    decode (Gen.Segment Gen.Hole  _   ) = return Name.Hole