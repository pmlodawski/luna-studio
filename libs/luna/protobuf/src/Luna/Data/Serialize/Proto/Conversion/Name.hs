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

import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Name.Name        as Gen
import qualified Generated.Proto.Name.Segment     as Gen
import qualified Generated.Proto.Name.Segment.Cls as Gen
import           Luna.DEP.AST.Name                (Name (Name))
import qualified Luna.DEP.AST.Name                as Name



instance Convert Name Gen.Name where
    encode (Name base segments) = Gen.Name (encodePJ base) (encode segments)
    decode (Gen.Name base segments) =
        Name <$> decodePJ base (missing "Name" "base")
             <*> decode segments


instance Convert Name.Segment Gen.Segment where
    encode (Name.Token str) = Gen.Segment Gen.Token $ encodePJ str
    encode  Name.Hole       = Gen.Segment Gen.Hole    Nothing
    decode (Gen.Segment Gen.Token name) = Name.Token <$> decodePJ name (missing "Name.Segment" "name")
    decode (Gen.Segment Gen.Hole  _   ) = return Name.Hole
