---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PolyKinds #-} -- needed to handle poly kinded Just args

module Data.TypeLevel.Maybe where

import Data.Typeable
import Prelude (Show)
import qualified Prelude as P


data Just (a :: k) deriving (Typeable)
data Nothing       deriving (Typeable)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

type family FromMaybe a where
    FromMaybe (Just a) = a


type family FromMaybe' a where
    FromMaybe' (Just a) = a
    FromMaybe' a        = a
