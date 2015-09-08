---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Geom2D.Accelerate.Basic where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Tuple
import           Data.Array.Accelerate.Array.Sugar
import           Data.Typeable

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Prelude hiding (lift)



data Line a    = Line (Point2 a) (Point2 a) deriving (Eq, Ord, Show,Typeable)
type Polygon a = Vector (Point2 a)

--------------------------------------------------------------------------------
-- Line accelerate tuple instances # straight to the tuple with no intermediate (un)lifting
--------------------------------------------------------------------------------
instance Functor Line where
    fmap f (Line a b) = Line (fmap f a) (fmap f b)

instance Applicative Line where
    pure a = Line (pure a) (pure a)
    {-# INLINE pure #-}
    Line a b <*> Line d e = Line (a <*> d) (b <*> e)
    {-# INLINE (<*>) #-}

type instance EltRepr (Line a)  = EltRepr ((a, a), (a, a))
type instance EltRepr' (Line a) = EltRepr' ((a, a), (a, a))

instance Elt a => Elt (Line a) where
  eltType _ = eltType (undefined :: ((a,a),(a,a)))
  toElt l = case toElt l of
     ((a, b), (c, d)) -> Line (Point2 a b) (Point2 c d)
  fromElt (Line (Point2 a b) (Point2 c d)) = fromElt ((a, b), (c, d))

  eltType' _ = eltType' (undefined :: ((a,a),(a,a)))
  toElt' l = case toElt' l of
     ((a, b), (c, d)) -> Line (Point2 a b) (Point2 c d)
  fromElt' (Line (Point2 a b) (Point2 c d)) = fromElt' ((a, b), (c, d))

instance IsTuple (Line a) where
  type TupleRepr (Line a) = TupleRepr ((a,a),(a,a))
  fromTuple (Line (Point2 a b) (Point2 c d)) = fromTuple ((a,b),(c,d))
  toTuple t = case toTuple t of
     ((a, b), (c, d)) -> Line (Point2 a b) (Point2 c d)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Line a) where
  type Plain (Line a) = Line (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Line (Point2 a b) (Point2 c d)) = Exp $ Tuple $ NilTup `SnocTup`
                                          (Exp $ Tuple $ NilTup `SnocTup` lift a `SnocTup` lift b)
                                          `SnocTup`
                                          (Exp $ Tuple $ NilTup `SnocTup` lift c `SnocTup` lift d)

instance (Elt a, e ~ Exp a) => Unlift Exp (Line e) where
  unlift t = let
    p1 = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
    p2 = Exp $ ZeroTupIdx `Prj` t
    in Line
      (Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` p1) (Exp $ ZeroTupIdx `Prj` p1))
      (Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` p2) (Exp $ ZeroTupIdx `Prj` p2))
