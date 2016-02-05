---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.TupleList where

import           Prelude       hiding (head, length, reverse, tail)

import           Data.Typeable
import           GHC.TypeLits

----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

empty :: ()
empty = ()

push :: a -> b -> (a,b)
push = (,)

infixr 0 //
(//) = (,)

-- === Head ===

head :: (x,xs) -> x
head (x,_) = x

-- === Tail ===

class Tail lst el | lst -> el where
    tail :: lst -> el

instance Tail (x,()) x where
    tail (x,_) = x

instance Tail xs out => Tail (x,xs) out where
    tail (_,xs) = tail xs


-- === Length ===

class Length lst where
    length :: lst -> Int

instance Length () where
    length _ = 0

instance Length xs => Length (x,xs) where
    length (_,xs) = 1 + length xs


-- === Reverse ===

class Reverse lst lst' out | lst lst' -> out where
    reverse' :: lst -> lst' -> out

instance Reverse () x x where
    reverse' _ = id

instance Reverse xs (x, rst) out => Reverse (x,xs) rst out where
    reverse' (x,xs) a = reverse' xs (x,a)

reverse = flip reverse' ()


-- === GetEl ===

class GetEl (n :: Nat) t el | n t -> el where
    getEl :: Proxy n -> t -> el

instance (t~(x,xs), el~x) => GetEl 0 t el where
    getEl _ (x,_) = x

instance (t~(x,xs), GetEl (n-1) xs el) => GetEl n t el where
    getEl n (_,xs) = getEl (Proxy :: Proxy (n-1)) xs


-- === SetEl ===

class SetEl (n :: Nat) v t t' | n v t -> t' where
    setEl :: Proxy n -> v -> t -> t'

instance (t~(x,xs), t'~(v,xs)) => SetEl 0 v t t' where
    setEl _ v (_,xs) = (v,xs)

instance (t~(x,xs), SetEl (n-1) v xs t'', t'~(x,t'')) => SetEl n v t t' where
    setEl n v (x,xs) = (x, setEl (Proxy::Proxy (n-1)) v xs)


-- === Append ===

class Append v lst lst' | v lst -> lst' where
    append :: v -> lst -> lst'

instance Append v () (v,()) where
    append = (,)

instance Append v xs xs' => Append v (x,xs) (x,xs') where
    append v (x,xs) = (x, append v xs)

-- === Prepend ===

prepend = (,)

-- === curryTuple ===

curryTuple0 f () = f
curryTuple1 f (x,xs) = curryTuple0 (f x) xs
curryTuple2 f (x,xs) = curryTuple1 (f x) xs
curryTuple3 f (x,xs) = curryTuple2 (f x) xs
curryTuple4 f (x,xs) = curryTuple3 (f x) xs
curryTuple5 f (x,xs) = curryTuple4 (f x) xs
curryTuple6 f (x,xs) = curryTuple5 (f x) xs
curryTuple7 f (x,xs) = curryTuple6 (f x) xs
curryTuple8 f (x,xs) = curryTuple7 (f x) xs
curryTuple9 f (x,xs) = curryTuple8 (f x) xs
curryTuple10 f (x,xs) = curryTuple9 (f x) xs
curryTuple11 f (x,xs) = curryTuple10 (f x) xs
curryTuple12 f (x,xs) = curryTuple11 (f x) xs


-- === UncurryTuple ===

-- |converts function taking a tuple list as argument into standard haskell one
--  eg. `(a,(b,(c,()))) -> out` into `a -> b -> c -> out`
--class UncurryTuple f out | f -> out where
--    uncurryTuple :: f -> out

--instance UncurryTuple (() -> a) a where
--    uncurryTuple f = f ()

--instance UncurryTuple (xs -> f) fout => UncurryTuple ((x,xs) -> f) (x -> fout) where
--    uncurryTuple f = (\x -> uncurryTuple $ f . (x,))


-- === TupleApp ===

-- |applies tuple list to a function
--  eg. tupleApp f (1,(2,(3,())))
class TupleApp f a out | f a -> out where
    tupleApp :: f -> a -> out

instance TupleApp f () f where
    tupleApp = const

instance (f~(a->b), TupleApp b xs out) => TupleApp f (a,xs) out where
    tupleApp f (x,xs) = (f x) `tupleApp` xs



--main = do
--    print $ getEl (Proxy::Proxy 1) (0,(1,(2,(3,(4,(5,()))))))
--    print $ setEl (Proxy::Proxy 1) 5 (0,(1,(2,(3,(4,(5,()))))))
