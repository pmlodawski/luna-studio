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

module Flowbox.Graphics.Color.CMYK where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude



data CMYK a = CMYK { cmykC :: a, cmykM :: a, cmykY :: a, cmykK :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (CMYK a) (CMYK a) a a where
    each f (CMYK c m y k) = CMYK <$> f c <*> f m <*> f y <*> f k
    {-# INLINE each #-}

type instance EltRepr (CMYK a)  = EltRepr (a, a, a, a)
type instance EltRepr' (CMYK a) = EltRepr' (a, a, a, a)

instance Elt a => Elt (CMYK a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y', z, w) -> CMYK x y' z w
  fromElt (CMYK x y' z w) = fromElt (x, y', z, w)

  eltType' _ = eltType' (undefined :: (a,a,a,a))
  toElt' p = case toElt' p of
     (x, y', z, w) -> CMYK x y' z w
  fromElt' (CMYK x y' z w) = fromElt' (x, y', z, w)

instance IsTuple (CMYK a) where
  type TupleRepr (CMYK a) = TupleRepr (a,a,a,a)
  fromTuple (CMYK x y' z w) = fromTuple (x,y',z,w)
  toTuple t = case toTuple t of
     (x, y', z, w) -> CMYK x y' z w

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (CMYK a) where
  type Plain (CMYK a) = CMYK (Plain a)
  lift (CMYK x y' z w) = Exp $ Tuple $ NilTup `SnocTup`
                        lift x `SnocTup`
                        lift y' `SnocTup`
                        lift z `SnocTup`
                        lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (CMYK e) where
  unlift t = CMYK (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                  (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                  (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                  (Exp $ ZeroTupIdx `Prj` t)
