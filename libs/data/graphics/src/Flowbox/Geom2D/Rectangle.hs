---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}

module Flowbox.Geom2D.Rectangle where

import           Flowbox.Prelude
import           Math.Coordinate.Cartesian (Point2 (..))

data Rectangle a = Rectangle { pA             :: Point2 a
                   			 , pB :: Point2 a
                             } deriving (Eq, Ord, Show)

pattern Rect xA yA xB yB = Rectangle (Point2 xA yA) (Point2 xB yB)

instance Functor Rectangle where
    fmap f (Rectangle a b) = Rectangle (fmap f a) (fmap f b)

properRect :: Ord a => Rectangle a -> Rectangle a
properRect (Rect xA yA xB yB) = Rectangle (Point2 (min xA xB) (min yA yB)) (Point2 (max xA xB) (max yA yB))

--instance Applicative Path where
--    pure a = Path (pure a) False
--    {-# INLINE pure #-}
--    Path a <*> Path b = Path (a <*> b)
--    {-# INLINE (<*>) #-}
