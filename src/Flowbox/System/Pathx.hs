

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

module Flowbox.System.Path where

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

import qualified Data.Map as Map
--import Control.Monad.Trans.Either
import Data.List (intercalate)
import GHC.Exts (Constraint)
import Control.Monad.State hiding (withState)
import Control.Applicative hiding (empty)

#include "ghcplatform.h"




type family If (cond :: Bool) (a :: k) (b :: k) where
  If True  a b = a
  If False a b = b

type family (a :: k) :== (b :: k) where
    a :== a = True
    a :== b = False


---------------------
(.:) = (.) . (.)


------------------------------------------------------------------------
-- Named
------------------------------------------------------------------------

data Named (name :: Symbol) a = Named a deriving (Show)


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

type family Insert val (set :: k) :: k
type instance Insert v ()    = (v,())
type instance Insert v (a,b) = Case (CmpNatRep v a)
                                   [ EQ :-> (v,b)
                                   , GT :-> (a, Insert v b)
                                   , LT :-> (v, (a,b))
                                   ]


type instance Insert v '[]      = '[v]
type instance Insert v (a ': b) = Case (CmpNatRep v a)
                                      [ EQ :-> v ': b
                                      , GT :-> a ': Insert v b
                                      , LT :-> v ': (a ': b)
                                      ]

type family CmpNatRep a b :: Ordering
type instance CmpNatRep a b = Cmp (NatRep a) (NatRep b)

class CmpNatRepCls a b ord | a b -> ord
instance (ord ~ Cmp (NatRep a) (NatRep b)) => CmpNatRepCls a b ord

-------------- where

type family ToSet (lst :: [k]) :: [k]
type instance ToSet '[] = '[]
type instance ToSet (a ': as) = Insert a (ToSet as)

--------------
-- utils

withState f = do
    s <- get
    put (f s)

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------


------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

data Rec a = Rec a deriving (Show)



type family NatRep (a :: k) :: [Nat]

type family MkRecord name variants where
    MkRecord name variants = Named name (RecordTemplate (ToSet variants))

type VariantCons t r = ConsByIdx (Index (NatRep t) (VariantReprsOf r)) t r

-- === Utils ===

cons :: forall a cls idx. (idx ~ Index (NatRep a) (VariantReprsOf cls), ConsByIdx idx a cls)
     => a -> cls
cons = consByIdx (Proxy :: Proxy idx)

-- === Instances ===

--instance KnownSymbol name => Show (Named name (Rec a)) where
--    show _ = "MkRecord " <> symbolVal (Proxy :: Proxy name)


------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type family VariantsOf a     :: [*]
type family VariantReprsOf a :: [[Nat]]

class VariantOf rec variant where
    withVariant :: rec -> (variant -> out) -> Maybe out

class ConsByIdx (idx :: Maybe Nat) a cls where
    consByIdx :: Proxy idx -> a -> cls

-- === ConsByIdx Instances ===

-- Basic handlers

instance ConsByIdx (Just n) a r => ConsByIdx (Just n) a (Rec r) where
    consByIdx = Rec .: consByIdx

instance ConsByIdx (Just n) a r => ConsByIdx (Just n) a (Named name r) where
    consByIdx = Named .: consByIdx

-- Maybe support

instance ConsByIdx Nothing a (Maybe r) where
    consByIdx _ _ = Nothing

instance ConsByIdx (Just n) a r => ConsByIdx (Just n) a (Maybe r) where
    consByIdx = Just .: consByIdx

-- Failure assertion

class InvalidRecordType a b | a -> b
instance InvalidRecordType r a => ConsByIdx Nothing a r where consByIdx = undefined

-- VariantOf

instance VariantOf a variant => VariantOf (Named n a) variant where
    withVariant (Named a) = withVariant a

instance VariantOf a variant => VariantOf (Rec a) variant where
    withVariant (Rec a) = withVariant a


------------------------------------------------------------------------
-- Pattern Matching
------------------------------------------------------------------------

--- === Match ===

newtype Match rec out = Match { runMatch :: rec -> Maybe out }

instance Show (Match rec out) where show _ = "Match"

--- === MatchSet ===

type MatchSet rec out = State [Match rec out] ()

class IsMatchSet a rec out | a -> rec out where
    toMatchSet :: a -> MatchSet rec out

instance IsMatchSet (MatchSet rec out) rec out where toMatchSet = id
instance IsMatchSet [MatchSet rec out] rec out where toMatchSet lst = sequence lst *> pure ()

-- === Utils ===

match :: (VariantOf rec variant) => (variant -> out) -> MatchSet rec out
match m = withState (<> [Match $ flip withVariant m])

matchSet :: [Match rec out] -> MatchSet rec out
matchSet = put

runCase :: rec -> MatchSet rec out -> Maybe out
runCase val s = case execState s mempty of
    (m:ms) -> case runMatch m val of
        Nothing -> runCase val $ matchSet ms
        Just a  -> Just a
    []     -> Nothing

secureCase :: (IsMatchSet matches rec out)
           => rec -> matches -> Maybe out
secureCase rec matches = runCase rec $ toMatchSet matches

unsecureCase :: (IsMatchSet matches rec out)
              => rec -> matches -> out
unsecureCase = fromJust .: secureCase

vcase :: (IsMatchSet matches rec out)
      => rec -> matches -> out
vcase = unsecureCase


------------------------------------------------------------------------
-- Casting
------------------------------------------------------------------------

class Cast r r' where
    cast :: r -> r'

instance (Cast a r) => Cast (Named name a) r where
    cast (Named a) = cast a


------------------------------------------------------------------------
-- MkRecord templates
------------------------------------------------------------------------

data R1 a = R1_V1 a
    deriving (Show)

data R2 a b
    = R2_V1 a
    | R2_V2 b
    deriving (Show)

data R3 a b c
    = R3_V1 a
    | R3_V2 b
    | R3_V3 c
    deriving (Show)


-- === Instances ===

-- Casting

instance (VariantCons t1 r) => Cast (R1 t1) r where
    cast = \case
        R1_V1 a -> cons a

instance (VariantCons t1 r, VariantCons t2 r) => Cast (R2 t1 t2) r where
    cast = \case
        R2_V1 a -> cons a
        R2_V2 a -> cons a

instance (VariantCons t1 r, VariantCons t2 r, VariantCons t3 r) => Cast (R3 t1 t2 t3) r where
    cast = \case
        R3_V1 a -> cons a
        R3_V2 a -> cons a
        R3_V3 a -> cons a

-- MkRecord templates
type family RecordTemplate (variants :: [k]) :: *
type instance RecordTemplate '[t1] = R1 t1
type instance RecordTemplate '[t1,t2] = R2 t1 t2
type instance RecordTemplate '[t1,t2,t3] = R3 t1 t2 t3

-- ConsByIdx
instance ConsByIdx (Just 0) t1 (R1 t1) where consByIdx _ = R1_V1

instance ConsByIdx (Just 0) t1 (R2 t1 t2) where consByIdx _ = R2_V1
instance ConsByIdx (Just 1) t2 (R2 t1 t2) where consByIdx _ = R2_V2

instance ConsByIdx (Just 0) t1 (R3 t1 t2 t3) where consByIdx _ = R3_V1
instance ConsByIdx (Just 1) t2 (R3 t1 t2 t3) where consByIdx _ = R3_V2
instance ConsByIdx (Just 2) t3 (R3 t1 t2 t3) where consByIdx _ = R3_V3

-- VariantsOf
type instance VariantsOf (Rec a) = VariantsOf a
type instance VariantsOf (R1 t1) = '[t1]
type instance VariantsOf (R2 t1 t2) = '[t1,t2]
type instance VariantsOf (R3 t1 t2 t3) = '[t1,t2,t3]

-- VariantReprsOf
type instance VariantReprsOf (Maybe a)     = VariantReprsOf a
type instance VariantReprsOf (Named n a)   = VariantReprsOf a
type instance VariantReprsOf (Rec a)       = VariantReprsOf a
type instance VariantReprsOf (R1 t1)       = '[NatRep t1]
type instance VariantReprsOf (R2 t1 t2)    = '[NatRep t1, NatRep t2]
type instance VariantReprsOf (R3 t1 t2 t3) = '[NatRep t1, NatRep t2, NatRep t3]

-- VariantOf
instance VariantOf (R1 t1) t1 where
    withVariant r f = case r of
        R1_V1 a -> Just $ f a
        _       -> Nothing

instance VariantOf (R2 t1 t2) t1 where
    withVariant r f = case r of
        R2_V1 a -> Just $ f a
        _       -> Nothing

instance VariantOf (R2 t1 t2) t2 where
    withVariant r f = case r of
        R2_V2 a -> Just $ f a
        _       -> Nothing

instance VariantOf (R3 t1 t2 t3) t1 where
    withVariant r f = case r of
        R3_V1 a -> Just $ f a
        _       -> Nothing

instance VariantOf (R3 t1 t2 t3) t2 where
    withVariant r f = case r of
        R3_V2 a -> Just $ f a
        _       -> Nothing

instance VariantOf (R3 t1 t2 t3) t3 where
    withVariant r f = case r of
        R3_V3 a -> Just $ f a
        _       -> Nothing




------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

data A = A Int deriving (Show, Typeable)
data B = B Int deriving (Show, Typeable)
data C = C Int deriving (Show, Typeable)

type instance NatRep A = [1,2,3]
type instance NatRep B = [1,2,4]
type instance NatRep C = [1,3,2]

type Foo1 = MkRecord "Foo1" '[A]
type Foo2 = MkRecord "Foo2" '[A,B]
type Foo3 = MkRecord "Foo3" '[A,B,C]

type Foo2' = MkRecord "Foo2" '[B,A]

type Foo1' = Named "Foo1" (R1 A)

class TestC a where
    testC :: a -> String

instance TestC Foo1' where
    testC _ = "x"

type Record2 name variants = Named name (RecordTemplate (ToSet variants))

--type X1 = Insert B ()
--type X2 = Insert A X1
--type X3 = Insert C X2

data Rec2 a = Rec2 (RecordTemplate (ToSet a))


foo = Rec2 (R2_V2 (A 1))
main = do
    --let x1 = insert (B 1) empty :: X1
    --    x2 = insert (A 2) x1    :: X2
    --    x3 = insert (C 1) x2    :: X3
    --print x3
    --print "---"
    --print $ typeOf (undefined :: X1)
    --print $ typeOf (undefined :: X2)
    --print $ typeOf (undefined :: X3)
    print "---"
    let x1 = cons (A 1) :: Foo1
        x2 = cons (B 1) :: Foo2
        x3 = cons (C 1) :: Foo3
        x4 = cons (C 1) :: Maybe Foo2

        y1 = cast x1 :: Foo1
        y2 = cast x2 :: Maybe Foo3
        y3 = cast x3 :: Maybe Foo1

    print $ x1
    print $ x2
    print $ x3
    print $ x4

    print "---"
    print $ y1
    print $ y2
    print $ y3

    print "---"
    let tst = cons (A 1) :: Foo2
        tst' = tst :: Foo2'

    print $ vcase tst $ do
        match $ \(A a) -> "a"
        match $ \(B a) -> "b"
    print "end"


