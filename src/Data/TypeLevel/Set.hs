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

module Data.TypeLevel.Set where

import GHC.TypeLits
import Data.Typeable
import Data.TypeLevel.Bool
import Prelude hiding (lookup)

type Empty = ()

type family Insert t set where
  Insert t ()    = (t,())
  Insert t (t,x) = (t,x)
  Insert t (a,x) = (a,Insert t x)

type family Remove t set where
  Remove t (t,x) = x
  Remove t (a,x) = (a,Remove t x)

type family Contains set t where
  Contains ()    t = False
  Contains (t,x) t = True
  Contains (a,x) t = Contains x t

type family Union set1 set2 where
  Union s ()    = s
  Union s (t,x) = Union (Insert t s) x

type family Difference set1 set2 where
  Difference s ()    = s
  Difference s (t,x) = Difference (Remove' t s) x

type family XOR set1 set2 where
  XOR s ()     = s
  XOR s (x,xs) = IfThenElse (Contains s x) (XOR (Remove x s) xs) (x, XOR s xs)

type family IfThenElse cond a b where
  IfThenElse True  a b = a
  IfThenElse False a b = b

type family IsSubset set1 set2 where
  IsSubset () s    = True
  IsSubset (t,x) s = And (Contains s t) (IsSubset x s)

type family Intersection set1 set2 where
  Intersection s () = ()
  Intersection s (t,x) = IfThenElse (Contains s t) (t, Intersection s x) (Intersection s x)

type family IsEmpty set where
  IsEmpty () = True
  IsEmpty a  = False

type family Size set where
  Size ()     = Proxy 0
  Size (x,xs) = SumProxy (Proxy 1) (Size xs)

type family SumProxy a b where
  SumProxy (Proxy a) (Proxy b) = (Proxy (a+b))

type IsSuperset a b = IsSubset b a

type Remove' t set = IfThenElse (Contains set t) (Remove t set) set


class InsertClass a s1 s2 | a s1 -> s2
    where insert :: a -> s1 -> s2

instance InsertClass a () (a,()) where
    insert a _ = (a,())

instance InsertClass a (a,xs) (a,xs) where
    insert a (_,xs) = (a,xs)

instance InsertClass a xs out => InsertClass a (x,xs) (x,out) where
    insert a (x,xs) = (x, insert a xs)


class Lookup s a where 
    lookup :: s -> Maybe a

instance Lookup () a where
    lookup _ = Nothing

instance Lookup (a,xs) a where
    lookup (x,_) = Just x

instance Lookup xs a => Lookup (x,xs) a where
    lookup (_,xs) = lookup xs


--teq :: a -> a -> a
--teq = const

--type A = Insert (Proxy 1) (Insert (Proxy 2) (Insert (Proxy 3) ()))
--type B = Insert (Proxy 2) (Insert (Proxy 3) (Insert (Proxy 4) ()))

--main = do
--   -- let x = (undefined :: Proxy '("ala", ()))
--   printType $ (undefined :: Insert (Proxy "ala2") (Insert (Proxy "ala") (Insert (Proxy "ala2") ())))
--   printType $ (undefined :: Proxy (2 + 2))

--   printType $ (undefined :: XOR A B)

--   -- let x = (undefined :: Proxy '[Int, Int, "ala"])
--   print "end"
