---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flowbox.Graphics.Composition.Generators.Structures where

import Flowbox.Prelude

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Typeable                     (Typeable)
import Math.Coordinate.Cartesian         (Point2(..))
import Math.Space.Space                  (Grid(..))



-- == Generator type ==

newtype Generator a = Generator {
    runGenerator :: Point2 (Exp Double) -> Grid (Exp Double) -> a
} deriving Functor

instance Applicative Generator where
    pure v = Generator $ \_ _ -> v
    Generator f <*> Generator gen = Generator $ \pixel space -> f pixel space (gen pixel space)

-- == Gradient tick type ==

data Tick a = Tick { _position :: a
                   , _value    :: a
                   , _weight   :: a
                   } deriving (Show, Read, Eq, Typeable)

instance Ord a => Ord (Tick a) where
    compare (Tick p1 _ _) (Tick p2 _ _) = compare p1 p2

makeLenses ''Tick

type instance EltRepr (Tick a)  = EltRepr (a, a, a)
type instance EltRepr' (Tick a) = EltRepr' (a, a, a)

instance Elt a => Elt (Tick a) where
    eltType _ = eltType (undefined :: (a,a,a))
    toElt p = case toElt p of
        (x, y, z) -> Tick x y z
    fromElt (Tick x y z) = fromElt (x, y, z)

    eltType' _ = eltType' (undefined :: (a,a,a))
    toElt' p = case toElt' p of
        (x, y, z) -> Tick x y z
    fromElt' (Tick x y z) = fromElt' (x, y, z)

instance IsTuple (Tick a) where
    type TupleRepr (Tick a) = TupleRepr (a,a,a)
    fromTuple (Tick x y z) = fromTuple (x,y,z)
    toTuple t = case toTuple t of
        (x, y, z) -> Tick x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Tick a) where
    type Plain (Tick a) = Tick (Plain a)
    --lift = Exp . Tuple . F.foldl SnocTup NilTup
    lift (Tick x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (Tick e) where
    unlift t = Tick (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                    (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                    (Exp $ ZeroTupIdx `Prj` t)
