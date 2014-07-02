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

module Flowbox.Graphics.Color.YUV_HD.Accelerate where

import       Data.Array.Accelerate
import       Data.Array.Accelerate.Smart
import       Data.Array.Accelerate.Tuple
import       Data.Array.Accelerate.Array.Sugar

import       Flowbox.Graphics.Color.YUV_HD
import       Flowbox.Prelude



type instance EltRepr (YUV_HD a)  = EltRepr (a, a, a)
type instance EltRepr' (YUV_HD a) = EltRepr' (a, a, a)

instance Elt a => Elt (YUV_HD a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> YUV_HD x y z
  fromElt (YUV_HD x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> YUV_HD x y z
  fromElt' (YUV_HD x y z) = fromElt' (x, y, z)

instance IsTuple (YUV_HD a) where
  type TupleRepr (YUV_HD a) = TupleRepr (a,a,a)
  fromTuple (YUV_HD x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> YUV_HD x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (YUV_HD a) where
  type Plain (YUV_HD a) = YUV_HD (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (YUV_HD x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (YUV_HD e) where
  unlift t = YUV_HD (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                    (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                    (Exp $ ZeroTupIdx `Prj` t)
