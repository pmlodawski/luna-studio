---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}

module Flowbox.Geom2D.QuadraticBezier where

import Data.Typeable

import Math.Coordinate.Cartesian  (Point2(..))
import Flowbox.Prelude



data QuadraticBezier a = QuadraticBezier { quadraticC0 :: Point2 a
                                         , quadraticC1 :: Point2 a
                                         , quadraticC2 :: Point2 a
                                         } deriving (Eq, Ord, Show, Typeable)

instance Functor QuadraticBezier where
    fmap f (QuadraticBezier a b c) = QuadraticBezier (fmap f a) (fmap f b) (fmap f c)

instance Applicative QuadraticBezier where
    pure a = QuadraticBezier (pure a) (pure a) (pure a)
    {-# INLINE pure #-}
    QuadraticBezier a b c <*> QuadraticBezier e f g = QuadraticBezier (a <*> e) (b <*> f) (c <*> g)
    {-# INLINE (<*>) #-}
