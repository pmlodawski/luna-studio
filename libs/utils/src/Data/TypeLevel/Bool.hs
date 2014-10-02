---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.TypeLevel.Bool where

import           Data.Typeable
import qualified Prelude       as P


data True deriving Typeable
data False deriving Typeable


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

class Bool a where
  toBool :: a -> P.Bool

instance Bool True where
  toBool _ = P.True

instance Bool False where
  toBool _ = P.False


type family And a b where
  And True True = True
  And a    b    = False
