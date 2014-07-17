---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}

module Flowbox.Graphics.Composition.Generators.Structures (
    module Flowbox.Graphics.Composition.Generators.Structures,
    Point2
) where

import Flowbox.Prelude

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Typeable                     (Typeable)
import Math.Coordinate.Cartesian

-- == Generator type ==

newtype Generator a b = Generator {
    runGenerator :: Point2 a -> b
} deriving Functor

type ContinousGenerator = Generator (Exp Double)
type DiscreteGenerator = Generator (Exp Int)

instance Applicative (Generator a) where
    pure v = Generator $ \_ -> v
    Generator f <*> Generator gen = Generator $ \point -> f point (gen point)

instance Monad (Generator a) where
    return = pure
    Generator gen >>= f = Generator $ \point -> runGenerator (f $ gen point) point

instance Num b => Num (Generator a b) where
    (+) = liftA2 (+)
    {-# INLINE (+) #-}
    (-) = liftA2 (-)
    {-# INLINE (-) #-}
    (*) = liftA2 (*)
    {-# INLINE (*) #-}
    negate = fmap negate
    {-# INLINE negate #-}
    abs = fmap abs
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

transform :: (Point2 a -> Point2 t) -> Generator t b -> Generator a b
transform trans (Generator gen) = Generator $ \pixel -> gen (trans pixel)

-- == Gradient tick type ==

data Tick a b c = Tick { _position :: a
                       , _value    :: b
                       , _weight   :: c
                       } deriving (Show, Read, Typeable)

instance Eq a => Eq (Tick a b c) where
    Tick p1 _ _ == Tick p2 _ _ = p1 == p2

instance Ord a => Ord (Tick a b c) where
    compare (Tick p1 _ _) (Tick p2 _ _) = compare p1 p2

makeLenses ''Tick

type instance EltRepr (Tick a b c)  = EltRepr (a, b, c)
type instance EltRepr' (Tick a b c) = EltRepr' (a, b, c)

instance (Elt a, Elt b, Elt c) => Elt (Tick a b c) where
    eltType _ = eltType (undefined :: (a,b,c))
    toElt p = case toElt p of
        (x, y, z) -> Tick x y z
    fromElt (Tick x y z) = fromElt (x, y, z)

    eltType' _ = eltType' (undefined :: (a,b,c))
    toElt' p = case toElt' p of
        (x, y, z) -> Tick x y z
    fromElt' (Tick x y z) = fromElt' (x, y, z)

instance IsTuple (Tick a b c) where
    type TupleRepr (Tick a b c) = TupleRepr (a,b,c)
    fromTuple (Tick x y z) = fromTuple (x,y,z)
    toTuple t = case toTuple t of
        (x, y, z) -> Tick x y z

instance (Lift Exp a, Elt (Plain a), Lift Exp b, Elt (Plain b), Lift Exp c, Elt (Plain c)) => Lift Exp (Tick a b c) where
    type Plain (Tick a b c) = Tick (Plain a) (Plain b) (Plain c)
    lift (Tick x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, Elt b, Elt c, e ~ Exp a, f ~ Exp b, g ~ Exp c) => Unlift Exp (Tick e f g) where
    unlift t = Tick (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                    (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                    (Exp $ ZeroTupIdx `Prj` t)
