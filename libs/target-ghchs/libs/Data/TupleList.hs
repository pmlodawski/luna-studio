---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

!{-# LANGUAGE RightSideContexts #-}

module Data.TupleList  where

import Prelude hiding (head, tail, length, reverse)

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

class Tail lst el | lst -> el where
    tail :: lst -> el

class Length lst where
    length :: lst -> Int

class Reverse lst lst' out | lst lst' -> out where
    reverse' :: lst -> lst' -> out

reverse = flip reverse' ()

----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

empty :: ()
empty = ()

push :: a -> b -> (a,b)
push = (,)

head :: (x,xs) -> x
head (x,_) = x

infixr 0 //
(//) = (,)

----------------------------------------------------------------------------------
-- TupleApp
-- applies tuple list to a function
-- eg. tupleApp f (1,(2,(3,())))
----------------------------------------------------------------------------------

class TupleApp f a out | f a -> out where
    tupleApp :: f -> a -> out

instance TupleApp f () f where
    tupleApp = const

instance TupleApp f (a,xs) out <= (f~(a->b), TupleApp b xs out) where
    tupleApp f (x,xs) = (f x) `tupleApp` xs

----------------------------------------------------------------------------------
-- UncurryTuple
-- converts function taking a tuple list as argument into standard haskell one
-- eg. `(a,(b,(c,()))) -> out` into `a -> b -> c -> out`
----------------------------------------------------------------------------------

class UncurryTuple f out | f -> out where
    uncurryTuple :: f -> out

instance UncurryTuple (() -> a) a where
    uncurryTuple f = f ()

instance UncurryTuple ((x,xs) -> f) (x -> fout) <= UncurryTuple (xs -> f) fout where
    uncurryTuple f = (\x -> uncurryTuple $ f . (x,))


----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance Tail (x,()) x where
    tail (x,_) = x

instance Tail (x,xs) out <= Tail xs out where
    tail (_,xs) = tail xs

---

instance Length () where
    length _ = 0

instance Length (x,xs) <= Length xs where
    length (_,xs) = 1 + length xs

---

instance Reverse () x x where
    reverse' _ = id

instance Reverse (x,xs) rst out <= Reverse xs (x, rst) out where
    reverse' (x,xs) a = reverse' xs (x,a)



