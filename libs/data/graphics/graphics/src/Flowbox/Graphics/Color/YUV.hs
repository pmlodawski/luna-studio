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

module Flowbox.Graphics.Color.YUV where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)



data YUV a = YUV { yuvY :: a, yuvU :: a, yuvV :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (YUV a) (YUV a) a a where
    each f (YUV y u v) = YUV <$> f y <*> f u <*> f v
    {-# INLINE each #-}

type instance EltRepr (YUV a)  = EltRepr (a, a, a)
type instance EltRepr' (YUV a) = EltRepr' (a, a, a)

instance Elt a => Elt (YUV a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y', z) -> YUV x y' z
  fromElt (YUV x y' z) = fromElt (x, y', z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y', z) -> YUV x y' z
  fromElt' (YUV x y' z) = fromElt' (x, y', z)

instance IsTuple (YUV a) where
  type TupleRepr (YUV a) = TupleRepr (a,a,a)
  fromTuple (YUV x y' z) = fromTuple (x,y',z)
  toTuple t = case toTuple t of
     (x, y', z) -> YUV x y' z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (YUV a) where
  type Plain (YUV a) = YUV (Plain a)
  lift (YUV x y' z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y' `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (YUV e) where
  unlift t = YUV (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                 (Exp $ ZeroTupIdx `Prj` t)
