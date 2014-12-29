---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.Rectangle where

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Prelude

data Rectangle a = Rectangle { pA :: Point2 a
                   			 , pB :: Point2 a
                             } deriving (Eq, Ord, Show)

instance Functor Rectangle where
    fmap f (Rectangle a b) = Rectangle (fmap f a) (fmap f b)

--instance Applicative Path where
--    pure a = Path (pure a) False
--    {-# INLINE pure #-}
--    Path a <*> Path b = Path (a <*> b)
--    {-# INLINE (<*>) #-}
