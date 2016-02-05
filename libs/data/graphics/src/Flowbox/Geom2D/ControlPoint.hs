---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Geom2D.ControlPoint where

import           Data.Binary               (Binary)

import           Flowbox.Prelude
import           Math.Coordinate.Cartesian (Point2 (..))



data ControlPoint a = ControlPoint { controlPoint :: Point2 a
                                   , handleIn     :: Maybe (Point2 a)
                                   , handleOut    :: Maybe (Point2 a)
                                   } deriving (Eq, Generic, Ord, Show)

instance Functor ControlPoint where
    fmap f (ControlPoint a b c) = ControlPoint (fmap f a) ((fmap.fmap) f b) ((fmap.fmap) f c)

instance Applicative ControlPoint where
    pure a = ControlPoint (pure a) ((pure.pure) a) ((pure.pure) a)
    {-# INLINE pure #-}
    -- f (a -> b) -> f a -> f b
    --ControlPoint a b c <*> ControlPoint e f g = ControlPoint (a <*> e) (b <*> f) (c <*> g)
    ControlPoint a b c <*> ControlPoint d e f = ControlPoint (a <*> d) p q
        where check r = case r of
                  Nothing -> False
                  Just _  -> True
              extract i' j' = let
                      Just i = i'
                      Just j = j'
                  in i <*> j
              handle i j = if (check i) && (check j)
                  then Just $ extract i j
                  else Nothing
              p = handle b e
              q = handle c f

    {-# INLINE (<*>) #-}

instance Binary a => Binary (Point2 a)
instance Binary a => Binary (ControlPoint a)


-- (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
