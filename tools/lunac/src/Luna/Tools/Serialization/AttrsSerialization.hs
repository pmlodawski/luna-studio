---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.AttrsSerialization where

import qualified Attrs_Types
import qualified Luna.Network.Flags as LFlags
import qualified Luna.Network.Attributes as LAttributes
import           Luna.Tools.Serialization

instance Serialize LFlags.Flags Attrs_Types.Flags where
  encode a = Attrs_Types.Flags (Just $ LFlags.io a) (Just $ LFlags.omit a)
  decode b = undefined


instance Serialize LAttributes.Attributes Attrs_Types.Attributes where
  encode a = undefined
  decode b = undefined