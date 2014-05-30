---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Flowbox.Graphics.Color.CMY.Accelerate where

import       Data.Array.Accelerate
import       Data.Array.Accelerate.Smart
import       Data.Array.Accelerate.Tuple
import       Data.Array.Accelerate.Array.Sugar

import       Flowbox.Graphics.Color.CMY
import       Flowbox.Prelude



type instance EltRepr (CMY a)  = EltRepr (a, a, a)
type instance EltRepr' (CMY a) = EltRepr' (a, a, a)

instance Elt a => Elt (CMY a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y', z) -> CMY x y' z
  fromElt (CMY x y' z) = fromElt (x, y', z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y', z) -> CMY x y' z
  fromElt' (CMY x y' z) = fromElt' (x, y', z)

instance IsTuple (CMY a) where
  type TupleRepr (CMY a) = TupleRepr (a,a,a)
  fromTuple (CMY x y' z) = fromTuple (x,y',z)
  toTuple t = case toTuple t of
     (x, y', z) -> CMY x y' z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (CMY a) where
  type Plain (CMY a) = CMY (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (CMY x y' z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y' `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (CMY e) where
  unlift t = CMY (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
