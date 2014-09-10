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

module Flowbox.Graphics.Color.CIE.Luv where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)



data Luv a = Luv { luvL :: a, luvU :: a, luvV :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (Luv a) (Luv b) a b where
    each f (Luv x y z) = Luv <$> f x <*> f y <*> f z
    {-# INLINE each #-}

type instance EltRepr (Luv a)  = EltRepr (a, a, a)
type instance EltRepr' (Luv a) = EltRepr' (a, a, a)

instance Elt a => Elt (Luv a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> Luv x y z
  fromElt (Luv x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> Luv x y z
  fromElt' (Luv x y z) = fromElt' (x, y, z)

instance IsTuple (Luv a) where
  type TupleRepr (Luv a) = TupleRepr (a,a,a)
  fromTuple (Luv x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> Luv x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Luv a) where
  type Plain (Luv a) = Luv (Plain a)
  lift (Luv x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (Luv e) where
  unlift t = Luv (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
