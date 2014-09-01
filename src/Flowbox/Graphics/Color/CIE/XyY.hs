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

module Flowbox.Graphics.Color.CIE.XyY where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)



data XyY a = XyY { xyYx :: a, xyYy :: a, xyYY :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (XyY a) (XyY b) a b where
    each f (XyY x y y') = XyY <$> f x <*> f y <*> f y'
    {-# INLINE each #-}

type instance EltRepr (XyY a)  = EltRepr (a, a, a)
type instance EltRepr' (XyY a) = EltRepr' (a, a, a)

instance Elt a => Elt (XyY a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> XyY x y z
  fromElt (XyY x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> XyY x y z
  fromElt' (XyY x y z) = fromElt' (x, y, z)

instance IsTuple (XyY a) where
  type TupleRepr (XyY a) = TupleRepr (a,a,a)
  fromTuple (XyY x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> XyY x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (XyY a) where
  type Plain (XyY a) = XyY (Plain a)
  lift (XyY x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (XyY e) where
  unlift t = XyY (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
