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

module Flowbox.Graphics.Composition.Generators.Structures where

import Flowbox.Prelude hiding            (transform)

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Typeable                     (Typeable)
import Data.Profunctor

import           Math.Coordinate.Coordinate
import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Space.Space

-- == Generator type ==

data Generator a b = Generator { canvas :: Grid (Exp Int)
                               , runGenerator :: a -> b
                               } deriving Functor

type CartesianGenerator a = Generator (Cartesian.Point2 a)
type DiscreteGenerator = CartesianGenerator (Exp Int)
type ContinousGenerator = CartesianGenerator (Exp Double)

unitGenerator a = Generator 1 a

--instance Applicative (Generator a) where
--    pure v = Generator $ \_ -> v
--    Generator f <*> Generator gen = Generator $ \point -> f point (gen point)

--instance Monad (Generator a) where
--    return = pure
--    Generator gen >>= f = Generator $ \point -> runGenerator (f $ gen point) point

--instance Num b => Num (Generator a b) where
--    (+) = liftA2 (+)
--    {-# INLINE (+) #-}
--    (-) = liftA2 (-)
--    {-# INLINE (-) #-}
--    (*) = liftA2 (*)
--    {-# INLINE (*) #-}
--    negate = fmap negate
--    {-# INLINE negate #-}
--    abs = fmap abs
--    {-# INLINE abs #-}
--    signum = fmap signum
--    {-# INLINE signum #-}
--    fromInteger = pure . fromInteger
--    {-# INLINE fromInteger #-}

instance Profunctor Generator where
    lmap f (Generator cnv gen) = Generator cnv $ gen . f
    rmap = fmap

transform :: (a -> t) -> Generator t b -> Generator a b
transform = lmap

-- == Coord conversions ==
--instance ( CoordConversion convType sys space (h a) (f a)
--         , CoordConversion convType sys space (g a) (h a)
--         ) => CoordConversion convType sys space (Generator (f a) b) (Generator (g a) b) where
--    convertCoordBase _ sys space = transform (convertCoordBase ManualConversion sys space)

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
