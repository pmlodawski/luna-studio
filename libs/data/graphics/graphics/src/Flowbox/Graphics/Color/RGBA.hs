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

module Flowbox.Graphics.Color.RGBA where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude



data RGBA a = RGBA { rgbaR :: a, rgbaG :: a, rgbaB :: a, rgbaA :: a }
			deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (RGBA a) (RGBA a) a a where
    each f (RGBA r g b a) = RGBA <$> f r <*> f g <*> f b <*> f a
    {-# INLINE each #-}

type instance EltRepr (RGBA a)  = EltRepr (a, a, a, a)
type instance EltRepr' (RGBA a) = EltRepr' (a, a, a, a)

instance Elt a => Elt (RGBA a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> RGBA x y z w
  fromElt (RGBA x y z w) = fromElt (x, y, z, w)

  eltType' _ = eltType' (undefined :: (a,a,a,a))
  toElt' p = case toElt' p of
     (x, y, z, w) -> RGBA x y z w
  fromElt' (RGBA x y z w) = fromElt' (x, y, z, w)

instance IsTuple (RGBA a) where
  type TupleRepr (RGBA a) = TupleRepr (a,a,a,a)
  fromTuple (RGBA x y z w) = fromTuple (x,y,z,w)
  toTuple t = case toTuple t of
     (x, y, z, w) -> RGBA x y z w

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGBA a) where
  type Plain (RGBA a) = RGBA (Plain a)
  lift (RGBA x y z w) = Exp $ Tuple $ NilTup `SnocTup`
                        lift x `SnocTup`
                        lift y `SnocTup`
                        lift z `SnocTup`
                        lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (RGBA e) where
  unlift t = RGBA (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                  (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                  (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                  (Exp $ ZeroTupIdx `Prj` t)
