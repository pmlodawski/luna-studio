

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
-- {-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DatatypeContexts #-}


module Flowbox.System.Variants where

--import           Control.Monad      ((>=>))
import Prelude
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
import           Flowbox.System.Types
import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)
import           GHC.Exts

#include "ghcplatform.h"



------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

data Record a where
    Record :: Eq (RecordTemplate a) => (RecordTemplate a) -> Record a

deriving instance Eq (Record a)

fromRecord :: Record a -> RecordTemplate a
fromRecord (Record a) = a


type family MkRecord variants where
    MkRecord variants = Record (ToSet variants)

-- === Utils ===

type family Variants a :: [*]
type family Reprs (a :: [*]) :: [[Nat]] where
    Reprs (v ': vs) = NatRep v ': Reprs vs
    Reprs '[]       = '[]

type instance Variants (Maybe a) = Variants a
type instance Variants (Record vars) = vars

-- === Instances ===

instance (IsSet a, VariantShow (RecordTemplate a)) => Show (Record a) where
    showsPrec d (Record a) = showParen (d > app_prec) $ showString $ "Record (" <> variantShow a <> ")"
         where app_prec = 10


------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type CheckConsByVariant variant m cons  = MaybeConsByIdx (variant `VariantIdx` cons) variant m cons
type MaybeConsByVariant variant m cons  = (CheckConsByVariant variant m cons, ToMaybe m)
--type SpecificCons      variant cons    = CheckConsByVariant variant Found cons

type VariantIdx         variant a       = NatRep variant `Index` VariantReprs a
type VariantReprs       variant         = Reprs (Variants variant)


-- === Variant constructors ===

data Found    a = Found { fromVal :: a } deriving (Show)
data NotFound a = NotFound deriving (Show)

instance ToMaybe Found      where toMaybe = Just . fromVal
instance ToMaybe NotFound where toMaybe _ = Nothing

------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------

-- === Construction functions ===

type Constructor      variant r = SmartConstructor variant (Record r)
type MaybeConstructor variant r = SmartConstructor variant (Maybe (Record r))

class SmartConstructor variant out where
    cons :: variant -> out

instance SpecificCons variant cons => SmartConstructor variant cons where
    cons = specificCons

instance MaybeConsByVariant variant m cons => SmartConstructor variant (Maybe cons) where
    cons = maybeCons


wrappedCons :: forall variant m cons. CheckConsByVariant variant m cons
            => variant -> m cons
wrappedCons = maybeConsByIdx (Proxy :: Proxy (variant `VariantIdx` cons))

maybeCons :: MaybeConsByVariant variant m cons => variant -> Maybe cons
maybeCons = toMaybe . wrappedCons

class SpecificCons variant cons where
    specificCons :: variant -> cons


-- === Construction utiities ===

-- MaybeConsByIdx

class MaybeConsByIdx (idx :: Maybe Nat) variant m cons | idx -> m where
    maybeConsByIdx :: Proxy idx -> variant -> m cons

instance (m ~ NotFound) => MaybeConsByIdx Nothing a m cons where
    maybeConsByIdx _ _ = NotFound

instance (m ~ Found, ConsByIdx n a cons) => MaybeConsByIdx ('Just n) a m cons where
    maybeConsByIdx _ = Found . consByIdx (Proxy :: Proxy n)

-- ConsByIdx

class ConsByIdx (idx :: Nat) variant cons where
    consByIdx :: Proxy idx -> variant -> cons

instance (ConsByIdx n a tmpl, Eq tmpl, tmpl ~ RecordTemplate r)
      => ConsByIdx n a (Record r) where
    consByIdx = Record .: consByIdx

-- SpecificCons

instance (CheckConsByVariant variant m cons, InvalidRecordType m variant cons) => SpecificCons variant cons where
    specificCons v = unpackValidRecord v $ wrappedCons v

-- Failure assertion

class InvalidRecordType m variant cons where
    unpackValidRecord :: variant -> m cons -> cons

instance InvalidRecordType Found variant cons where
    unpackValidRecord _ = fromVal


------------------------------------------------------------------------
-- Pattern Matching
------------------------------------------------------------------------

--- === Match ===

newtype Match rec out = Match { runMatch :: rec -> Maybe out } deriving (Typeable)

instance (Typeable rec, Typeable out) => Show (Match rec out) where show _ = show (typeOf (undefined :: Match rec out))

--- === MatchSet ===

type MatchSet rec out = State [Match rec out] ()

class IsMatchSet a rec out | a -> rec out where
    toMatchSet :: a -> MatchSet rec out

instance IsMatchSet (MatchSet rec out) rec out where toMatchSet     = id
instance IsMatchSet [MatchSet rec out] rec out where toMatchSet lst = sequence lst *> pure ()
instance IsMatchSet [Match rec out]    rec out where toMatchSet     = put
instance (Typeable rec, Typeable out) => Show (MatchSet rec out) where
    show _ = "MatchSet " ++ show (typeOf (undefined :: [Match rec out]))

-- === Utils ===

match :: VariantMap v out rec => (v -> out) -> MatchSet rec out
match m = withState (<> [Match $ mapVariant m])

runCase :: rec -> MatchSet rec out -> Maybe out
runCase val s = case execState s mempty of
    (m:ms) -> runMatch m val `orElse` runCase val (toMatchSet ms)
    []     -> Nothing

secureCase :: IsMatchSet matches rec out
           => rec -> matches -> Maybe out
secureCase rec matches = runCase rec $ toMatchSet matches

unsecureCase :: IsMatchSet matches rec out
              => rec -> matches -> out
unsecureCase = fromJust .: secureCase

case' :: IsMatchSet matches rec out
      => rec -> matches -> out
case' = unsecureCase


------------------------------------------------------------------------
-- Casting
------------------------------------------------------------------------

type IsSecureCast a b = IsSubset (VariantReprs a) (VariantReprs b)
type WrappedCast  a m b = ( m ~ CastOutput (IsSecureCast a b)
                          , WithVariantsM (Cast' (IsSecureCast a b)) a m b
                          )

type family CastOutput (secure :: Bool) :: (* -> *) where
    CastOutput False = Maybe
    CastOutput True  = Found

class Cast a b where
    cast :: a -> b

-- Variant casting

class Cast' (validCast :: Bool) a m b | validCast -> m where
    cast' :: Proxy validCast -> a -> m b

instance (CheckConsByVariant a Found b) => Cast' True a Found b where
    cast' _ = wrappedCons

instance (CheckConsByVariant a m b, ToMaybe m) => Cast' False a Maybe b where
    cast' _ = toMaybe . wrappedCons


-- cast types

wrappedCast :: forall secureCast a m b. (WrappedCast a m b, secureCast ~ IsSecureCast a b)
            => a -> m b
wrappedCast = withVariantsM (Proxy :: Proxy (Cast' secureCast)) $ cast' (Proxy :: Proxy secureCast)

instance (WrappedCast a (CastOutput (IsSecureCast a b)) b', UnpackCast (CastOutput (IsSecureCast a b) b') b)
      => Cast a b where
    cast = unpackCast . wrappedCast

-- cast unpacking

class UnpackCast a b | b -> a where
    unpackCast :: a -> b

instance UnpackCast (Maybe a) (Maybe a) where
    unpackCast = id

instance (a ~ Found b) => UnpackCast a b where
    unpackCast = fromVal


------------------------------------------------------------------------
-- Record bulk processing
------------------------------------------------------------------------

class WithVariantsM ctx a m b where
    withVariantsM :: Proxy ctx -> (forall v. ctx v m b => v -> m b) -> a -> m b

class WithVariants ctx a b where
    withVariants :: Proxy ctx -> (forall v. ctx v b => v -> b) -> a -> b

--

instance (WithVariantsM ctx (RecordTemplate vars) m out)
      => WithVariantsM ctx (Record vars) m out where
    withVariantsM ctx f (Record a) = withVariantsM ctx f a

instance (WithVariants ctx (RecordTemplate vars) out)
      => WithVariants ctx (Record vars) out where
    withVariants ctx f (Record a) = withVariants ctx f a


------------------------------------------------------------------------
-- Variant Utilities
------------------------------------------------------------------------

-- === VariantShow ===

type VariantShow rec = WithVariants VariantsShow rec String

class (out ~ String) => VariantsShow v out where
    variantsShow :: v -> out

instance Show v => VariantsShow v String where
    variantsShow = show

variantShow :: VariantShow rec => rec -> String
variantShow   = withVariants (Proxy :: Proxy VariantsShow) variantsShow


-- === Variant map ===

data ANY = ANY deriving (Show, Typeable)

class (m ~ Maybe, b~out) => MapVariant' a b v m out where
    mapVariant' :: (a -> b) -> v -> m out

instance MapVariant' a   out a Maybe out where mapVariant' f a = Just $ f a
instance MapVariant' a   out b Maybe out where mapVariant' _ _ = Nothing
instance MapVariant' ANY out b Maybe out where mapVariant' f _ = Just $ f ANY

type VariantMap a b rec = WithVariantsM (MapVariant' a b) rec Maybe b

mapVariant :: VariantMap a b rec => (a -> b) -> rec -> Maybe b
mapVariant (f :: (a -> b)) = withVariantsM (Proxy :: Proxy (MapVariant' a b)) (mapVariant' f)

------------------------------------------------------------------------
-- MkRecord templates
------------------------------------------------------------------------

type family TemplateCtxsM' ctx ts (m :: * -> *) a :: Constraint where
    TemplateCtxsM' ctx '[] m a = ()
    TemplateCtxsM' ctx (t ': ts) m a = (ctx t m a, TemplateCtxsM' ctx ts m a)

type TemplateCtxsM ctx t m a = TemplateCtxsM' ctx (Variants t) m a

type family TemplateCtxs' ctx ts a :: Constraint where
    TemplateCtxs' ctx '[] a = ()
    TemplateCtxs' ctx (t ': ts) a = (ctx t a, TemplateCtxs' ctx ts a)

type TemplateCtxs ctx t a = TemplateCtxs' ctx (Variants t) a

---

data R1 t1 = R1_V1 t1
    deriving (Show, Eq)

data R2 t1 t2
    = R2_V1 t1
    | R2_V2 t2
    deriving (Show, Eq)

data R3 t1 t2 t3
    = R3_V1 t1
    | R3_V2 t2
    | R3_V3 t3
    deriving (Show, Eq)

data R4 t1 t2 t3 t4
   = R4_V1 t1
   | R4_V2 t2
   | R4_V3 t3
   | R4_V4 t4
   deriving (Show, Eq)

data R5 t1 t2 t3 t4 t5
   = R5_V1 t1
   | R5_V2 t2
   | R5_V3 t3
   | R5_V4 t4
   | R5_V5 t5
   deriving (Show, Eq)


-- === Instances ===

instance TemplateCtxsM ctx (R1 t1) m a
      => WithVariantsM ctx (R1 t1) m a where
    withVariantsM _ f = \case
        R1_V1 a -> f a

instance TemplateCtxsM ctx (R2 t1 t2) m a
      => WithVariantsM ctx (R2 t1 t2) m a where
    withVariantsM _ f = \case
        R2_V1 a -> f a
        R2_V2 a -> f a

instance TemplateCtxsM ctx (R3 t1 t2 t3) m a
      => WithVariantsM ctx (R3 t1 t2 t3) m a where
    withVariantsM _ f = \case
        R3_V1 a -> f a
        R3_V2 a -> f a
        R3_V3 a -> f a

instance TemplateCtxsM ctx (R4 t1 t2 t3 t4) m a
      => WithVariantsM ctx (R4 t1 t2 t3 t4) m a where
    withVariantsM _ f = \case
        R4_V1 a -> f a
        R4_V2 a -> f a
        R4_V3 a -> f a
        R4_V4 a -> f a

instance TemplateCtxsM ctx (R5 t1 t2 t3 t4 t5) m a
      => WithVariantsM ctx (R5 t1 t2 t3 t4 t5) m a where
    withVariantsM _ f = \case
        R5_V1 a -> f a
        R5_V2 a -> f a
        R5_V3 a -> f a
        R5_V4 a -> f a
        R5_V5 a -> f a

---

instance TemplateCtxs ctx (R1 t1) a
      => WithVariants ctx (R1 t1) a where
    withVariants _ f = \case
        R1_V1 a -> f a

instance TemplateCtxs ctx (R2 t1 t2) a
      => WithVariants ctx (R2 t1 t2) a where
    withVariants _ f = \case
        R2_V1 a -> f a
        R2_V2 a -> f a

instance TemplateCtxs ctx (R3 t1 t2 t3) a
      => WithVariants ctx (R3 t1 t2 t3) a where
    withVariants _ f = \case
        R3_V1 a -> f a
        R3_V2 a -> f a
        R3_V3 a -> f a

instance TemplateCtxs ctx (R4 t1 t2 t3 t4) a
      => WithVariants ctx (R4 t1 t2 t3 t4) a where
    withVariants _ f = \case
        R4_V1 a -> f a
        R4_V2 a -> f a
        R4_V3 a -> f a
        R4_V4 a -> f a

instance TemplateCtxs ctx (R5 t1 t2 t3 t4 t5) a
      => WithVariants ctx (R5 t1 t2 t3 t4 t5) a where
    withVariants _ f = \case
        R5_V1 a -> f a
        R5_V2 a -> f a
        R5_V3 a -> f a
        R5_V4 a -> f a
        R5_V5 a -> f a




-- MkRecord templates
type family RecordTemplate (variants :: [*]) :: *
--type instance RecordTemplate '[] = R0
type instance RecordTemplate '[t1] = R1 t1
type instance RecordTemplate '[t1,t2] = R2 t1 t2
type instance RecordTemplate '[t1,t2,t3] = R3 t1 t2 t3
type instance RecordTemplate '[t1,t2,t3,t4] = R4 t1 t2 t3 t4
type instance RecordTemplate '[t1,t2,t3,t4,t5] = R5 t1 t2 t3 t4 t5


instance ConsByIdx 0 t1 (R1 t1) where consByIdx _ = R1_V1

instance ConsByIdx 0 t1 (R2 t1 t2) where consByIdx _ = R2_V1
instance ConsByIdx 1 t2 (R2 t1 t2) where consByIdx _ = R2_V2

instance ConsByIdx 0 t1 (R3 t1 t2 t3) where consByIdx _ = R3_V1
instance ConsByIdx 1 t2 (R3 t1 t2 t3) where consByIdx _ = R3_V2
instance ConsByIdx 2 t3 (R3 t1 t2 t3) where consByIdx _ = R3_V3

instance ConsByIdx 0 t1 (R4 t1 t2 t3 t4) where consByIdx _ = R4_V1
instance ConsByIdx 1 t2 (R4 t1 t2 t3 t4) where consByIdx _ = R4_V2
instance ConsByIdx 2 t3 (R4 t1 t2 t3 t4) where consByIdx _ = R4_V3
instance ConsByIdx 3 t4 (R4 t1 t2 t3 t4) where consByIdx _ = R4_V4

instance ConsByIdx 0 t1 (R5 t1 t2 t3 t4 t5) where consByIdx _ = R5_V1
instance ConsByIdx 1 t2 (R5 t1 t2 t3 t4 t5) where consByIdx _ = R5_V2
instance ConsByIdx 2 t3 (R5 t1 t2 t3 t4 t5) where consByIdx _ = R5_V3
instance ConsByIdx 3 t4 (R5 t1 t2 t3 t4 t5) where consByIdx _ = R5_V4
instance ConsByIdx 4 t5 (R5 t1 t2 t3 t4 t5) where consByIdx _ = R5_V5

-- Variants
type instance Variants (R1 t1) = '[t1]
type instance Variants (R2 t1 t2) = '[t1,t2]
type instance Variants (R3 t1 t2 t3) = '[t1,t2,t3]
type instance Variants (R4 t1 t2 t3 t4) = '[t1,t2,t3,t4]
type instance Variants (R5 t1 t2 t3 t4 t5) = '[t1,t2,t3,t4,t5]



------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

data A = A Int deriving (Show, Eq, Typeable)
data B = B Int deriving (Show, Eq, Typeable)
data C = C Int deriving (Show, Eq, Typeable)

type instance NatRep A = [1,2,3]
type instance NatRep B = [1,2,4]
type instance NatRep C = [1,3,2]

type Foox = Record '[A]


type Foo1 = MkRecord '[A]
type Foo2 = MkRecord '[A,B]
type Foo3 = MkRecord '[A,B,C]

type Foo2' = MkRecord '[B,A]

ofType :: a -> a -> a
ofType = const

-- main = do
--     let x1 = cons (A 1) :: Foo1
--         x2 = cons (B 1) :: Foo2
--         x3 = cons (C 1) :: Foo3
--         x4 = cons (A 1) :: Maybe Foo3
--         x5 = cons (C 1) :: Maybe Foo3
--         x6 = cons (C 1) :: Maybe Foo1
--
--         --x7 = cons (B 1) :: Foo1
--
--
--     print $ x1
--     print $ x2
--     print $ x3
--     print $ x4
--     print $ x5
--     print $ x6
--     print "---"
--
--     let y1 = cast x1 :: Foo1
--         y2 = cast x2 :: Foo3
--         y3 = cast x2 :: Maybe Foo1
--         --y4 = cast x2 :: Foo1
--
--     print $ y1
--     print $ y2
--     print $ y3
--     print "---"
--
--     let x1'  = specificCons (A 1) :: Foo1
--         x1'' = wrappedCons (A 1) :: Found Foo1
--         x2'' = wrappedCons (B 1) :: NotFound Foo1
--
--
--     let tst = cons (B 1) :: Foo2
--         tst' = tst :: Foo2'
--
--     print $ case' tst $ do
--         match $ \(A a) -> "a"
--         --match $ \(B a) -> "b"
--         match $ \ANY   -> "x"
--     print "==="
--     putStrLn ""
