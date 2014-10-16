---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Data.Serialize.Proto.Conversion.Attributes where

import qualified Data.Foldable as Foldable
import qualified Data.Map      as Map
import qualified Data.Sequence as Sequence

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Attributes.Attributes                as Gen
import qualified Generated.Proto.Attributes.Attributes.Space          as Gen
import qualified Generated.Proto.Attributes.Attributes.Space.KeyValue as Gen
import qualified Generated.Proto.Attributes.Flags                     as Gen
import qualified Generated.Proto.Attributes.Properties                as Gen
import           Luna.Graph.Attributes                                (Attributes)
import qualified Luna.Graph.Attributes                                as Attributes
import           Luna.Graph.Flags                                     (Flags (Flags))
import           Luna.Graph.Properties                                (Properties (Properties))



instance Convert Flags Gen.Flags where
    encode (Flags omit astFolded astAssignment graphFolded defaultNodeGenerated graphViewGenerated position) =
        Gen.Flags (Just omit) astFolded astAssignment graphFolded defaultNodeGenerated graphViewGenerated (fmap fst position) (fmap snd position)
    decode (Gen.Flags momit astFolded astAssignment graphFolded defaultNodeGenerated graphViewGenerated  mpositionX mpositionY) = do
        omit <- momit <?> "Failed to decode Flags: 'omit' field is missing"
        let position = (,) <$> mpositionX <*> mpositionY
        return $ Flags omit astFolded astAssignment graphFolded defaultNodeGenerated graphViewGenerated  position


instance ConvertPure Attributes Gen.Attributes where
    encodeP attributes = Gen.Attributes $ Sequence.fromList $ map encodeP $ Attributes.toList attributes
    decodeP (Gen.Attributes tspaces) = Attributes.fromList $ map decodeP $ Foldable.toList tspaces


instance ConvertPure (String, Attributes.Map String String) Gen.Space where
    encodeP (k, v) = Gen.Space (encodeP k) $ Sequence.fromList $ map encodeP $ Map.toList v
    decodeP (Gen.Space tk tv) = (decodeP tk, Map.fromList $ map decodeP $ Foldable.toList tv)


instance ConvertPure (String, String) Gen.KeyValue where
    encodeP (k, v) = Gen.KeyValue (encodeP k) (encodeP v)
    decodeP (Gen.KeyValue tk tv) = (decodeP tk, decodeP tv)


instance Convert Properties Gen.Properties where
    encode (Properties flags attributes) = Gen.Properties (encodeJ flags) (encodePJ attributes)
    decode (Gen.Properties mflags mattributes) = do
        flags      <- mflags      <?> "Failed to decode Properties: 'flags' field is missing"
        attributes <- mattributes <?> "Failed to decode Properties: 'attributes' field is missing"
        Properties <$> decode flags <*> pure (decodeP attributes)
