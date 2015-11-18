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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

--{-# LANGUAGE DysfunctionalDependencies #-}


module Data.TypeLevel.Map (
    module Data.TypeLevel.Map
)where

import GHC.TypeLits
import Data.Typeable
import Data.TypeLevel.Bool
--import Data.TypeLevel.FlatContainers as X
import Prelude hiding (lookup, Eq, head, tail)
import Flowbox.Utils 

type Empty = ()
empty = ()

type family RecKey rec
type family RecVal rec

type instance RecKey (k,v) = k
type instance RecVal (k,v) = v

class Record rec k v | rec -> k v where
    key :: rec -> k
    val :: rec -> v

instance Record (a,b) a b where
    key (x,_) = x
    val (_,x) = x


class Head lst el | lst -> el where
    head :: lst -> el

class Tail lst els | lst -> els where
    tail :: lst -> els


type family Insert k v dict where
  Insert k v ()        = ((k,v),())
  Insert k v (idx,x)   = If (Eq k (RecKey idx)) ((k,v),x) (idx,Insert k v x)

type family Remove k dict where
  Remove k ()        = ()
  Remove k (idx,x)   = If (Eq k (RecKey idx)) x (idx,Remove k x)


class InsertClass k v dict out | k v dict -> out where
    insert :: k -> v -> dict -> out

instance InsertClass k v () ((k,v),()) where
    insert k v _ = ((k,v),())

instance InsertClass k v ((k,w),x) ((k,v),x) where
    insert k v (_,x) = ((k,v),x)

instance InsertClass k v x out => InsertClass k v ((j,w),x) ((j,w),out) where
    insert k v (idx,x) = (idx, insert k v x)


class RemoveClass k dict out | k dict -> out where
    remove :: k -> dict -> out

instance RemoveClass k () () where
    remove _ = id

instance RemoveClass k ((k,w),x) x where
    remove _ (_,x) = x

instance RemoveClass k x out => RemoveClass k ((j,w),x) ((j,w),out) where
    remove k (idx, x) = (idx, remove k x)


class RemoveClass2 k dict out | k dict -> out where
    remove2 :: k -> dict -> out

instance (Record idx ik iv, RemoveFirstIf matchel (idx, x) out, matchel ~ Eq k ik) 
      => RemoveClass2 k (idx, x) out where
    remove2 key dict = removeFirstIf (undefined :: matchel) dict

class RemoveFirstIf cond dict out | cond dict -> out where
    removeFirstIf :: cond -> dict -> out

instance RemoveFirstIf False dict dict where
    removeFirstIf _ = id

instance Tail dict out => RemoveFirstIf True dict out where
    removeFirstIf _ = tail

--data A = A deriving (Typeable, Show)
--data B = B deriving (Typeable, Show)
--data C = C deriving (Typeable, Show)

--data V1 = V1 deriving (Typeable, Show)
--data V2 = V2 deriving (Typeable, Show)
--data V3 = V3 deriving (Typeable, Show)


----type X = Insert C V3 (Insert B V2 (Insert A V1 ()))

--main = do
--    let d = insert A V1 $ insert B V2 $ ()
--    print d
----   -- let x = (undefined :: Proxy '("ala", ()))
----   printType $ (undefined :: Insert (Proxy "ala2") (Insert (Proxy "ala") (Insert (Proxy "ala2") ())))
--   printType $ (undefined :: X) -- insert A V2 $ insert A V1 ()

----   printType $ (undefined :: XOR A B)

----   -- let x = (undefined :: Proxy '[Int, Int, "ala"])
----   print "end"
