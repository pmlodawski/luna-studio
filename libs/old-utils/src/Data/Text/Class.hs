---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Class where

import           Data.Text.Lazy (Text, pack, unpack)
import           Prelude


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: Text -> a

class (ToText a, FromText a) => IsText a


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance ToText Text where
    toText = id

instance FromText Text where
    fromText = id

instance IsText Text

---

instance ToText String where
    toText = pack

instance FromText String where
    fromText = unpack

instance IsText String

