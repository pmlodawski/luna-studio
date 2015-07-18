---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE UndecidableInstances #-}

module Data.String.Repr  where

import Prelude

----------------------------------------------------------------------
-- Tpye classes
----------------------------------------------------------------------

class StrRepr a where
    strRepr :: a -> String


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance StrRepr String where
    strRepr = id

instance StrRepr Char where
    strRepr a = [a]

instance {-# OVERLAPPABLE #-} Show a => StrRepr a where
    strRepr = show

