---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Geom2D.Path where

import Data.Binary (Binary)

import Flowbox.Geom2D.ControlPoint
import Flowbox.Prelude



data Path a = Path { isClosed      :: Bool
                   , controlPoints :: [ControlPoint a]
                   } deriving (Eq, Ord, Show, Generic)


instance Functor Path where
    fmap f (Path closed points) = Path closed $ (fmap.fmap) f points

instance Binary a => Binary (Path a)

--instance Applicative Path where
--    pure a = Path (pure a) False
--    {-# INLINE pure #-}
--    Path a <*> Path b = Path (a <*> b)
--    {-# INLINE (<*>) #-}
