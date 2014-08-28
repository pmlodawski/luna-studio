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

module Flowbox.Graphics.Color.HSL where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude



data HSL a = HSL { hslH :: a, hslS :: a, hslL :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (HSL a) (HSL a) a a where
    each f (HSL h s l) = HSL <$> f h <*> f s <*> f l
    {-# INLINE each #-}

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
  lift (HSL x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (HSL e) where
  unlift t = HSL (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
