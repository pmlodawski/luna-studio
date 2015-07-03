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

module Luna.DEP.Data.Serialize.Proto.Conversion.Attributes where

import qualified Data.Foldable as Foldable
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe
import qualified Data.Sequence as Sequence

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Attributes.Attributes                as Gen
import qualified Generated.Proto.Dep.Attributes.Attributes.Space          as Gen
import qualified Generated.Proto.Dep.Attributes.Attributes.Space.KeyValue as Gen
import qualified Generated.Proto.Dep.Attributes.Flags                     as Gen
import qualified Generated.Proto.Dep.Attributes.FoldInfo                  as Gen
import qualified Generated.Proto.Dep.Attributes.Properties                as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.NodeDefault     ()
import           Luna.DEP.Graph.Attributes                                (Attributes)
import qualified Luna.DEP.Graph.Attributes                                as Attributes
import           Luna.DEP.Graph.Flags                                     (Flags (Flags))
import qualified Luna.DEP.Graph.Flags                                     as Flags
import           Luna.DEP.Graph.Properties                                (Properties (Properties))



instance Convert Flags Gen.Flags where
    encode (Flags       omit  astFolded astAssignment graphFoldInfo grouped defaultNodeGenerated defaultNodeOriginID graphViewGenerated position) =
        Gen.Flags (Just omit) astFolded astAssignment (fmap encode graphFoldInfo) grouped defaultNodeGenerated (fmap encodeP defaultNodeOriginID) graphViewGenerated (fmap fst position) (fmap snd position)
    decode (Gen.Flags   momit astFolded astAssignment tgraphFoldInfo grouped defaultNodeGenerated tdefaultNodeOriginID graphViewGenerated  mpositionX mpositionY) = do
        omit <- momit <?> "Failed to decode Flags: 'omit' field is missing"
        let position = (,) <$> mpositionX <*> mpositionY
        graphFoldInfo       <- Maybe.maybe (return Nothing) (fmap Just . decode) tgraphFoldInfo
        let defaultNodeOriginID = decodeP <$> tdefaultNodeOriginID
        return $ Flags omit astFolded astAssignment graphFoldInfo grouped defaultNodeGenerated defaultNodeOriginID graphViewGenerated  position


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
    encode (Properties flags defautsMap attributes) =
        Gen.Properties (encodeJ flags) (encodeJ defautsMap) (encodePJ attributes)
    decode (Gen.Properties flags defaultsMap attributes) =
        Properties <$> decodeJ flags (missing "Properties" "flags")
                   <*> decodeJ defaultsMap (missing "Properties" "defaultsMap")
                   <*> decodePJ attributes (missing "Properties" "attributes")


instance Convert Flags.FoldInfo Gen.FoldInfo where
    encode  Flags.Folded     = Gen.FoldInfo (Just True) Nothing
    encode (Flags.FoldTop i) = Gen.FoldInfo Nothing $ encodePJ i
    decode (Gen.FoldInfo (Just True) _) = return Flags.Folded
    decode (Gen.FoldInfo _ (Just i)   ) = return $ Flags.FoldTop $ decodeP i
    decode _                            = Left "Failed to decode FoldInfo"
