---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs where

import           Flowbox.Prelude                   
import qualified Data.HashMap.Strict             as HashMap
import           Data.HashMap.Strict               (HashMap)
import           Data.Text.Lazy                    (Text, pack, unpack)

import qualified Attrs_Types                     as TAttrs
import           Flowbox.Control.Error             
import           Flowbox.Luna.Network.Flags        (Flags(Flags))
import qualified Flowbox.Luna.Network.Attributes as Attributes
import           Flowbox.Luna.Network.Attributes   (Attributes)
import           Flowbox.Tools.Conversion          


instance Convert Flags TAttrs.Flags where
    encode (Flags io omit) = TAttrs.Flags (Just io) (Just omit)
    decode (TAttrs.Flags (Just io) (Just omit)) = Right $ Flags io omit
    decode (TAttrs.Flags (Just _  ) Nothing     ) = Left "Failed to decode Flags: `omit` field is missing"
    decode (TAttrs.Flags {}                     ) = Left "Failed to decode Flags: `io` field is missing"


instance Convert Attributes TAttrs.Attributes where
    encode m = TAttrs.Attributes $ Just h where
        mItems  = Attributes.toList m
        mcItems = map convertItem mItems
        h       = HashMap.fromList mcItems

    decode (TAttrs.Attributes mtspaces) = do
        tspaces <- mtspaces <?> "Failed to decode Attributes: `spaces` field is missing"
        let hItems  = HashMap.toList tspaces
            hcItems = map convertItemBack hItems
        return $ Attributes.fromList hcItems


convertItem :: (String, Attributes.Map String String) -> (Text, HashMap Text Text)
convertItem (k, v) = (ck, cv) where
    ck = pack k
    cv = HashMap.fromList $ map (\(k1, v1) -> (pack k1, pack v1)) $ Attributes.toList v


convertItemBack :: (Text, HashMap Text Text) -> (String, Attributes.Map String String)
convertItemBack (k, v) = (ck, cv) where
    ck = unpack k
    cv = Attributes.fromList $ map (\(k1, v1) -> (unpack k1, unpack v1)) $ HashMap.toList v

