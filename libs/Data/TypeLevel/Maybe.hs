---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Data.TypeLevel.Maybe where

import Data.Typeable
import Prelude (Show)
import qualified Prelude as P


newtype Just a  = Just a deriving (Show, Typeable)
data    Nothing = Nothing deriving (Show, Typeable)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

type family FromMaybe a where
    FromMaybe (Just a) = a


type family FromMaybe' a where
    FromMaybe' (Just a) = a
    FromMaybe' a        = a
