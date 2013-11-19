---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes where

import           Control.Applicative                                    
import qualified Data.Foldable                                        as Foldable
import qualified Data.Map                                             as Map
import qualified Data.Sequence                                        as Sequence
import qualified Text.ProtocolBuffers.Basic                           as Proto

import           Flowbox.Prelude                                        
import           Flowbox.Luna.Network.Flags                             (Flags(Flags))
import qualified Flowbox.Luna.Network.Attributes                      as Attributes
import           Flowbox.Luna.Network.Attributes                        (Attributes)
import           Flowbox.Tools.Conversion.Proto                         
import qualified Generated.Proto.Attributes.Attributes                as Gen
import qualified Generated.Proto.Attributes.Attributes.Space          as Gen
import qualified Generated.Proto.Attributes.Attributes.Space.KeyValue as Gen
import qualified Generated.Proto.Attributes.Flags                     as Gen



instance Convert Flags Gen.Flags where
    encode (Flags io omit) = Gen.Flags (Just io) (Just omit)
    decode (Gen.Flags (Just io) (Just omit)) = Right $ Flags io omit
    decode (Gen.Flags (Just _  ) Nothing   ) = Left "Failed to decode Flags: `omit` field is missing"
    decode (Gen.Flags {}                   ) = Left "Failed to decode Flags: `io` field is missing"


instance Convert Attributes Gen.Attributes where
    encode attrs = Gen.Attributes  tattrs where
        tattrs = Sequence.fromList $ map encode $ Attributes.toList attrs
    decode (Gen.Attributes tspaces) = do
        attrs <- Attributes.fromList <$> (sequence $ map decode $ Foldable.toList tspaces)
        return attrs


instance Convert (String, Attributes.Map String String) Gen.Space where
    encode (k, v) = Gen.Space (Proto.uFromString k) tv where
        tv = Sequence.fromList $ map encode $ Map.toList v
    decode (Gen.Space tk tv) = do
        v <- Map.fromList <$> (sequence $ map decode $ Foldable.toList tv)
        return (Proto.uToString tk, v)


instance Convert (String, String) Gen.KeyValue where
    encode (k, v) = Gen.KeyValue (Proto.uFromString k) (Proto.uFromString v)
    decode (Gen.KeyValue tk tv) = return (Proto.uToString tk, Proto.uToString tv)
