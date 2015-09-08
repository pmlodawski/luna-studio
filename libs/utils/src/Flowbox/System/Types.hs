

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FunctionalDependencies            #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DeriveDataTypeable            #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE TypeFamilies            #-}

{-# LANGUAGE NoOverloadedStrings            #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE DefaultSignatures            #-}
{-# LANGUAGE FlexibleInstances            #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE EmptyDataDecls #-}


module Flowbox.System.Types where

--import           Control.Monad      ((>=>))
import           Data.Binary        (Binary)
import           Data.Typeable      hiding (cast)
--import           Data.List.Split    (splitOn)
--import qualified Data.List          as List
--import qualified Data.String.Utils  as StringUtils
import qualified System.Directory   as Directory
import qualified System.Environment as Env
import           Control.Exception  (catch, SomeException)
import           Control.Monad      (join)
--import           System.IO.Error    (IOError, isDoesNotExistError)
import           System.IO.Error    (tryIOError)
import           Data.Map           (Map)
import qualified Data.Map           as Map
--import           Control.Error.Safe (justErr)
--import           Data.Binary
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.Foldable (fold)

--import           Flowbox.Prelude                    hiding (empty, fromList, toList)
--import qualified Flowbox.System.Directory.Locations as Directory
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
--import Flowbox.Prelude hiding (cons)
import GHC.TypeLits

--import Control.Monad.Shuffle (deepBind, (>>>=), ($>>=))

--import Data.ConcreteTypeRep
import Control.Monad.Identity

import qualified Data.Map as Map
--import Control.Monad.Trans.Either
import Data.List (intercalate)
import GHC.Exts (Constraint)
import Control.Monad.State hiding (withState)
import Control.Applicative hiding (empty)

--------------------------

import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)
import           GHC.Exts
import Prelude
#include "ghcplatform.h"




type family If (cond :: Bool) (a :: k) (b :: k) where
  If True  a b = a
  If False a b = b

type family (a :: k) :== (b :: k) where
    a :== a = True
    a :== b = False


---------------------
(.:) = (.) . (.)

class ToMaybe m where
    toMaybe :: m a -> Maybe a

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y


------------------------------------------------------------------------
-- Named
------------------------------------------------------------------------




empty :: ()
empty = ()

------------------------------------------------------------------------
-- Type level
------------------------------------------------------------------------

infixl 1 :->
data (match :: k) :-> (val :: l)

type family Case (exp :: k) (vals :: [*]) :: l
type instance Case exp ((k :-> l) ': ss) = If (exp :== k) l (Case exp ss)


type family Cmp (a :: k) (b :: k) :: Ordering
type instance Cmp (a :: Nat) (b :: Nat) = CmpNat a b
type instance Cmp Int Int = EQ

type instance Cmp (a ': as) (b ': bs) = If (Cmp a b :== EQ) (Cmp as bs) (Cmp a b)
type instance Cmp '[] '[] = EQ


type family ToRTuple (a :: k) :: *

type instance ToRTuple '[] = ()
type instance ToRTuple (a ': as) = (a, ToRTuple as)

type family ToTList (a :: k) :: [l]

type instance ToTList () = '[]
type instance ToTList (a,b) = a ': ToTList b

------------------------------------------------------------------------
-- Type level Sets
------------------------------------------------------------------------

--class ToTSet (a :: [k]) (set :: [k]) | a -> set where
--    toTSet :: Proxy a -> Proxy set

type family Index (a :: k) (lst :: l) :: Maybe Nat
type instance Index a (b ': bs) = If (a :== b)
                                     (Just 0)
                                     (If (Index a bs :== Nothing)
                                         Nothing
                                         (Just (1 + FromJust (Index a bs)))
                                     )
type instance Index a '[] = Nothing

type family FromJust a where FromJust (Just a) = a

type family Inc a where Inc a = a + 1

type family Fmap (f :: v -> v) (a :: k) :: k
type instance Fmap f Nothing = Nothing
type instance Fmap f (Just a) = Just (f a)


class InsertCmpCls (cmp :: Ordering) val set out | cmp set val -> out where
    insertCmp :: Proxy cmp -> val -> set -> out

instance InsertCmpCls EQ v (a,b) (v,b) where
    insertCmp _ v (a,b) = (v,b)

instance InsertCls v b b' => InsertCmpCls GT v (a,b) (a,b') where
    insertCmp _ v (a,b) = (a, insert v b)

instance InsertCmpCls LT v (a,b) (v,(a,b)) where
    insertCmp _ = (,)

--instance InsertCmpCls EQ v (a,b)

class InsertCls val set out | val set -> out where
    insert :: val -> set -> out

instance InsertCls v () (v,()) where
    insert = (,)

instance (CmpNatRepCls v a cmp, InsertCmpCls cmp v (a,b) out) => InsertCls v (a,b) out where
    insert = insertCmp (Proxy :: Proxy cmp)

----

type family And a b where
    And True True = True
    And a    b    = False

type family InsertNatRep val (set :: k) :: k
type instance InsertNatRep v ()    = (v,())
type instance InsertNatRep v (a,b) = Case (CmpNatRep v a)
                                         [ EQ :-> (v,b)
                                         , GT :-> (a, InsertNatRep v b)
                                         , LT :-> (v, (a,b))
                                         ]


type instance InsertNatRep v '[]      = '[v]
type instance InsertNatRep v (a ': b) = Case (CmpNatRep v a)
                                            [ EQ :-> v ': b
                                            , GT :-> a ': InsertNatRep v b
                                            , LT :-> v ': (a ': b)
                                            ]

type family Insert val (set :: k) :: k
type instance Insert v '[]      = '[v]
type instance Insert v (a ': b) = Case (v :== a)
                                      [ True  :-> a ': b
                                      , False :-> a ': Insert v b
                                      ]


type family IsSubset (set :: k) (superset :: k) :: Bool
type instance IsSubset '[] sset = True
type instance IsSubset (a ': as) sset = (a `In` sset) `And` (as `IsSubset` sset)
---

type family Remove a (set :: k) :: k
type instance Remove v (a ': b) = If (v :== a) b (a ': Remove v b)
type instance Remove v (a,b)    = If (v :== a) b (a, Remove v b)

---

type family Diff (set :: k) (set' :: k) :: k
type instance Diff set (a ': b) = Diff (Remove a set) b
type instance Diff set '[]      = set

---

type family Union (set :: k) (set' :: k) :: k
type instance Union (a ': b) set = Union b (Insert a set)
type instance Union '[]      set = set


---

type family In (a :: l) (set :: k) :: Bool
type instance In v (a ': b) = If (v :== a) True (In v b)
type instance In v '[]      = False
type instance In v (a,b)    = If (v :== a) True (In v b)
type instance In v ()       = False

---

type family CmpNatRep a b :: Ordering
type instance CmpNatRep a b = Cmp (NatRep a) (NatRep b)

class CmpNatRepCls a b ord | a b -> ord
instance (ord ~ Cmp (NatRep a) (NatRep b)) => CmpNatRepCls a b ord

-------------- where

type family ToSet (lst :: [k]) :: [k]
type instance ToSet '[] = '[]
type instance ToSet (a ': as) = Insert a (ToSet as)

type IsSet a = (a ~ ToSet a)


--------------
-- utils

withState f = do
    s <- get
    put (f s)

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

type family NatRep (a :: *) :: [Nat]