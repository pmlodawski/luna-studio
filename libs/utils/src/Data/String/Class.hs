---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.String.Class (
    module Data.String.Class,
    module X
) where

import Prelude
import Data.String as X (IsString (fromString))


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class ToString a where
    toString   :: a -> String

    default toString :: Show a => a -> String
    toString = show


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance ToString String where
    toString = id

instance Show a => ToString a where
    toString = show

