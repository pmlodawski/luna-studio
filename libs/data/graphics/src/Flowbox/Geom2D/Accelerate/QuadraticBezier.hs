---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Flowbox.Geom2D.Accelerate.QuadraticBezier where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Tuple
import           Data.Array.Accelerate.Array.Sugar

import Math.Coordinate.Cartesian      (Point2(..))
import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Prelude                hiding (lift)



----------------------------------------------------------------------------------
---- QuadraticBezier accelerate tuple instances # straight to the tuple with no intermediate (un)lifting
----------------------------------------------------------------------------------
type instance EltRepr (QuadraticBezier a)  = EltRepr ((a, a), (a, a), (a, a))
type instance EltRepr' (QuadraticBezier a) = EltRepr' ((a, a), (a, a), (a, a))

instance Elt a => Elt (QuadraticBezier a) where
  eltType _ = eltType (undefined :: ((a,a),(a,a),(a,a)))
  toElt n = case toElt n of
     ((a, b), (c, d), (e, f)) -> QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)
  fromElt (QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)) = fromElt ((a, b), (c, d), (e, f))

  eltType' _ = eltType' (undefined :: ((a,a),(a,a),(a,a)))
  toElt' n = case toElt' n of
     ((a, b), (c, d), (e, f)) -> QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)
  fromElt' (QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)) = fromElt' ((a, b), (c, d), (e, f))

instance IsTuple (QuadraticBezier a) where
  type TupleRepr (QuadraticBezier a) = TupleRepr ((a,a),(a,a),(a,a))
  fromTuple (QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)) = fromTuple ((a, b), (c, d), (e, f))
  toTuple t = case toTuple t of
     ((a, b), (c, d), (e, f)) -> QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (QuadraticBezier a) where
  type Plain (QuadraticBezier a) = QuadraticBezier (Plain a)
  --  lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (QuadraticBezier (Point2 a b) (Point2 c d) (Point2 e f)) =
    Exp $ Tuple $ NilTup `SnocTup`
    (Exp $ Tuple $ NilTup `SnocTup` lift a `SnocTup` lift b)
     `SnocTup`
    (Exp $ Tuple $ NilTup `SnocTup` lift c `SnocTup` lift d)
    `SnocTup`
    (Exp $ Tuple $ NilTup `SnocTup` lift e `SnocTup` lift f)

instance (Elt a, e ~ Exp a) => Unlift Exp (QuadraticBezier e) where
  unlift t = let
    p1 = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
    p2 = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
    p3 = Exp $ ZeroTupIdx `Prj` t
    in QuadraticBezier
      (Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` p1) (Exp $ ZeroTupIdx `Prj` p1))
      (Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` p2) (Exp $ ZeroTupIdx `Prj` p2))
      (Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` p3) (Exp $ ZeroTupIdx `Prj` p3))
