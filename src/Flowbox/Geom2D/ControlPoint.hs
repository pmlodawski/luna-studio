---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.ControlPoint where

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Prelude

data ControlPoint a = ControlPoint { controlPoint :: Point2 a
                                   , handleIn     :: Point2 a
                                   , handleOut    :: Point2 a
                                   } deriving (Eq, Ord, Show)

instance Functor ControlPoint where
    fmap f (ControlPoint a b c) = ControlPoint (fmap f a) (fmap f b) (fmap f c)

instance Applicative ControlPoint where
    pure a = ControlPoint (pure a) (pure a) (pure a)
    {-# INLINE pure #-}
    ControlPoint a b c <*> ControlPoint e f g = ControlPoint (a <*> e) (b <*> f) (c <*> g)
    {-# INLINE (<*>) #-}
