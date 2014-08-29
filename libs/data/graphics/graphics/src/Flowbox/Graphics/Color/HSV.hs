---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Color.HSV where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)



data HSV a = HSV { hsvH :: a, hsvS :: a, hsvV :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (HSV a) (HSV a) a a where
    each f (HSV h s v) = HSV <$> f h <*> f s <*> f v
    {-# INLINE each #-}

type instance EltRepr (HSV a)  = EltRepr (a, a, a)
type instance EltRepr' (HSV a) = EltRepr' (a, a, a)

instance Elt a => Elt (HSV a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> HSV x y z
  fromElt (HSV x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> HSV x y z
  fromElt' (HSV x y z) = fromElt' (x, y, z)

instance IsTuple (HSV a) where
  type TupleRepr (HSV a) = TupleRepr (a,a,a)
  fromTuple (HSV x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> HSV x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (HSV a) where
  type Plain (HSV a) = HSV (Plain a)
  lift (HSV x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (HSV e) where
  unlift t = HSV (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
