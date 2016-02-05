---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Geom2D.Mask where

import           Data.Binary         (Binary)

import           Flowbox.Geom2D.Path
import           Flowbox.Prelude



data Mask a = Mask { path    :: Path a
                   , feather :: Maybe (Path a)
                   } deriving (Eq, Generic, Ord, Show)


instance Functor Mask where
    fmap f (Mask a b) = Mask (fmap f a) ((fmap.fmap) f b)

instance Binary a => Binary (Mask a)

--instance Applicative Shape where
--    pure a = Shape (pure a)
--    {-# INLINE pure #-}
--    Shape a <*> Shape b = Shape (a <*> b)
--    {-# INLINE (<*>) #-}
