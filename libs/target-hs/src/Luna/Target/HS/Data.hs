---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Luna.Target.HS.Data where

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

newtype Pure a = Pure a deriving (Eq, Ord, Generic, Typeable)
newtype Safe a = Safe a deriving (Eq, Ord, Generic, Typeable)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Functor Pure where
    fmap f (Pure a) = Pure $ f a


instance Functor Safe where
    fmap f (Safe a) = Safe $ f a


instance Show a => Show (Pure a) where
#ifdef DEBUG
    show (Pure a) = "Pure (" ++ child ++ ")" where
        child = show a
        content = if ' ' `elem` child then "(" ++ child ++ ")" else child
#else
    show (Pure a) = show a
#endif


instance Show a => Show (Safe a) where
#ifdef DEBUG
    show (Safe a) = "Safe " ++ content where
        child = show a
        content = if ' ' `elem` child then "(" ++ child ++ ")" else child
#else
    show (Safe a) = show a
#endif