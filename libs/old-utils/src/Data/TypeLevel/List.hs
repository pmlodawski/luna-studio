---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

--{-# LANGUAGE DysfunctionalDependencies #-}


module Data.TypeLevel.List (
    module X,
    module Data.TypeLevel.List
)where

import GHC.TypeLits
import Data.Typeable
import Data.TypeLevel.Bool
import Data.TypeLevel.FlatContainers as X
import Prelude hiding (lookup, reverse)

type Empty = ()
empty = ()

type family Append t lst where
  Append t ()     = (t,())
  Append t (x,xs) = (x, Append t xs)

type Prepend t lst = (t,lst)



newtype ListCons a = ListCons a deriving Show

buildStart = build' ()

class ListBuilder l r where
    build' :: l -> r

instance (l~l') => ListBuilder l (ListCons l') where
    build' = ListCons

instance ListBuilder (a, l) r => ListBuilder l (a -> r) where
    build' l a = build' (a,l)

buildEnd :: Reversable a b => ListCons a -> b
buildEnd (ListCons a) = reverse a


reverse lst = reverse' lst ()

type Reversable a b = ReverseClass a () b

class ReverseClass l m l' | l m -> l' where
    reverse' :: l -> m -> l'

instance ReverseClass () x x where
    reverse' _ = id

instance (ReverseClass b (a, x) y) => ReverseClass (a,b) x y where
    reverse' (x,xs) a = reverse' xs (x,a)


class App f args out | f args -> out where
    app :: f -> args -> out

instance App f () f where
    app = const

instance (f~(a -> f'), App f' as out ) => App f (a,as) out where
    app f (a,as) = app (f a) as



main = do
    let a = (1,(2,(3,())))
    print $ buildEnd $ buildStart 1 2 3
