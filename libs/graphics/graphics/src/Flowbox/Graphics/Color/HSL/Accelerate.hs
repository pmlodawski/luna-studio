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

module Flowbox.Graphics.Color.HSL.Accelerate where

import       Data.Array.Accelerate
import       Data.Array.Accelerate.Smart
import       Data.Array.Accelerate.Tuple
import       Data.Array.Accelerate.Array.Sugar

import       Flowbox.Graphics.Color.HSL
import       Flowbox.Prelude



type instance EltRepr (HSL a)  = EltRepr (a, a, a)
type instance EltRepr' (HSL a) = EltRepr' (a, a, a)

instance Elt a => Elt (HSL a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> HSL x y z
  fromElt (HSL x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> HSL x y z
  fromElt' (HSL x y z) = fromElt' (x, y, z)

instance IsTuple (HSL a) where
  type TupleRepr (HSL a) = TupleRepr (a,a,a)
  fromTuple (HSL x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> HSL x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (HSL a) where
  type Plain (HSL a) = HSL (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (HSL x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (HSL e) where
  unlift t = HSL (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
