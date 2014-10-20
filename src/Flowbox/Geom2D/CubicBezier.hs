---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}

module Flowbox.Geom2D.CubicBezier where

import Data.Typeable

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Prelude



data CubicBezier a = CubicBezier { cubicC0 :: Point2 a
                                 , cubicC1 :: Point2 a
                                 , cubicC2 :: Point2 a
                                 , cubicC3 :: Point2 a
                                 } deriving (Eq, Ord, Show, Typeable)

instance Functor CubicBezier where
    fmap f (CubicBezier a b c d) = CubicBezier (fmap f a) (fmap f b) (fmap f c) (fmap f d)

instance Applicative CubicBezier where
    pure a = CubicBezier (pure a) (pure a) (pure a) (pure a)
    {-# INLINE pure #-}
    CubicBezier a b c d <*> CubicBezier e f g h = CubicBezier (a <*> e) (b <*> f) (c <*> g) (d <*> h)
    {-# INLINE (<*>) #-}
