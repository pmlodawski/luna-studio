---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Attrs where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy      as Text

import qualified Attrs_Types
import           Luna.Network.Flags        (Flags(..))
import qualified Luna.Network.Attributes as Attributes
import           Luna.Network.Attributes   (Attributes(..))
import           Luna.Tools.Serialization


instance Serialize Flags Attrs_Types.Flags where
  encode (Flags io omit) = Attrs_Types.Flags (Just io) (Just omit)
  decode (Attrs_Types.Flags (Just io) (Just omit)) = Right $ Flags io omit
  decode (Attrs_Types.Flags (Just io) Nothing    ) = Left "`omit` field is missing"
  decode (Attrs_Types.Flags {}                   ) = Left "`io` field is missing"


instance Serialize Attributes Attrs_Types.Attributes where
  encode m = Attrs_Types.Attributes $ Just h where
                 mItems = Attributes.toList m
                 mcItems = map convertItem mItems
                 h = HashMap.fromList mcItems

  decode el = case el of
    (Attrs_Types.Attributes (Just h)) -> Right m where
                                            hItems = HashMap.toList h
                                            hcItems = map convertItemBack hItems
                                            m = Attributes.fromList hcItems
    (Attrs_Types.Attributes Nothing)  ->  Left "`map` field is missing"


convertItem (k, v) = (ck, cv) where
    ck = Text.pack k
    cv = HashMap.fromList $ map (\(k1, v1) -> (Text.pack k1, Text.pack v1)) $ Attributes.toList v

convertItemBack (k, v) = (ck, cv) where
    ck = Text.unpack k
    cv = Attributes.fromList $ map (\(k1, v1) -> (Text.unpack k1, Text.unpack v1)) $ HashMap.toList v

