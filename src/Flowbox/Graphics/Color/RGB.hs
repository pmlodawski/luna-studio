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

module Flowbox.Graphics.Color.RGB where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)



data RGB a = RGB { rgbR :: a, rgbG :: a, rgbB :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (RGB a) (RGB b) a b where
    each f (RGB r g b) = RGB <$> f r <*> f g <*> f b
    {-# INLINE each #-}

type instance EltRepr (RGB a)  = EltRepr (a, a, a)
type instance EltRepr' (RGB a) = EltRepr' (a, a, a)

instance Elt a => Elt (RGB a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> RGB x y z
  fromElt (RGB x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> RGB x y z
  fromElt' (RGB x y z) = fromElt' (x, y, z)

instance IsTuple (RGB a) where
  type TupleRepr (RGB a) = TupleRepr (a,a,a)
  fromTuple (RGB x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> RGB x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGB a) where
  type Plain (RGB a) = RGB (Plain a)
  lift (RGB x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (RGB e) where
  unlift t = RGB (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
