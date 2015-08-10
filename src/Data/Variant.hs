

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


module Data.Variant where

import           Data.Repr
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
import Flowbox.Prelude hiding (cons, Index, cast)
import           Flowbox.System.Types (ToMaybe, toMaybe, IsSubset, orElse, withState, ToSet, Index)
import           GHC.Generics       (Generic)
import           GHC.Exts

#include "ghcplatform.h"



------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

--data Record a where
--    Record :: Eq (RecordTemplate a) => (RecordTemplate a) -> Record a

data Record a = Record (RecordTemplate a)

--deriving instance Eq (Record a)

fromRecord :: Record a -> RecordTemplate a
fromRecord (Record a) = a


type family MkRecord variants where
    MkRecord variants = Record (ToSet variants)

-- === Utils ===

type family Variants a :: [*]
--type family Reprs (a :: [*]) :: [[Nat]] where
--    Reprs (v ': vs) = NatRep v ': Reprs vs
--    Reprs '[]       = '[]

type instance Variants (Maybe a) = Variants a
type instance Variants (Record vars) = vars

-- === Instances ===

instance (VariantShow (RecordTemplate a)) => Show (Record a) where
    showsPrec d (Record a) = showParen (d > app_prec) $ showString $ "Record (" <> variantShow a <> ")"
         where app_prec = 10

instance IsVariant (Record a) where variant = id
                                    record  = id

instance Repr (RecordTemplate a) => Repr (Record a) where
    repr (Record a) = repr a

------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type CheckConsByVariant variant m cons = ( MaybeConsByIdx (Index variant (Variants cons)) variant m (RecordTemplate (Variants cons))
                                         , IsVariant cons
                                         , Functor m
                                         )

type MaybeConsByVariant variant m cons  = (CheckConsByVariant variant m cons, ToMaybe m)

class IsVariant a where
    variant :: Record (Variants a) -> a
    record  :: Lens' a (Record (Variants a))

-- === Variant constructors ===

data Found    a = Found { fromVal :: a } deriving (Show, Functor)
data NotFound a = NotFound deriving (Show)

instance Functor NotFound where fmap _ _ = NotFound

instance ToMaybe Found      where toMaybe = Just . fromVal
instance ToMaybe NotFound where toMaybe _ = Nothing

------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------

-- === Construction functions ===

--type Constructor      variant r = Constructor variant (Record r)
type MaybeConstructor variant out = Constructor variant (Maybe out)

class Constructor variant out where
    cons :: variant -> out

instance Constructor a a where
    cons = id

instance SpecificCons variant cons => Constructor variant cons where
    cons = specificCons

instance MaybeConsByVariant variant m cons => Constructor variant (Maybe cons) where
    cons = maybeCons

wrappedCons :: forall variant m cons variants rec. (CheckConsByVariant variant m cons, variants ~ Variants cons, rec ~ (RecordTemplate variants))
            => variant -> m cons
wrappedCons v = variant . Record <$> (maybeConsByIdx (Proxy :: Proxy (variant `Index` variants)) v :: m rec)

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

instance (ConsByIdx n a tmpl, tmpl ~ RecordTemplate r)
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

newtype Match rec out = Match { runMatch :: Record rec -> Maybe out } deriving (Typeable)

instance (Typeable rec, Typeable out) => Show (Match rec out) where show _ = show (typeOf (undefined :: Match rec out))

--- === MatchSet ===

type MatchSet rec out = State [Match rec out] ()

type Case a matches out = (IsVariant a, IsMatchSet matches (Variants a) out)

class IsMatchSet matches a out | matches -> a out where
    toMatchSet :: matches -> MatchSet a out

instance IsMatchSet (MatchSet rec out) rec out where toMatchSet     = id
instance IsMatchSet [MatchSet rec out] rec out where toMatchSet lst = sequence lst *> pure ()
instance IsMatchSet [Match rec out]    rec out where toMatchSet     = put
instance (Typeable rec, Typeable out) => Show (MatchSet rec out) where
    show _ = "MatchSet " ++ show (typeOf (undefined :: [Match rec out]))

-- === Utils ===

match :: VariantMap v out (Record rec) => (v -> out) -> MatchSet rec out
match m = withState (<> [Match $ mapVariant m])

runCase :: Record rec -> MatchSet rec out -> Maybe out
runCase val s = case execState s mempty of
    (m:ms) -> runMatch m val `orElse` runCase val (toMatchSet ms)
    []     -> Nothing

secureCase :: IsMatchSet matches rec out
           => Record rec -> matches -> Maybe out
secureCase rec matches = runCase rec $ toMatchSet matches

unsecureCase :: IsMatchSet matches rec out
              => Record rec -> matches -> out
unsecureCase = fromJust .: secureCase

case' :: Case a matches out => a -> matches -> out
case' = unsecureCase . view record



------------------------------------------------------------------------
-- Casting
------------------------------------------------------------------------

type IsSecureCast a b = IsSubset (Variants a) (Variants b)
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

data R6 t1 t2 t3 t4 t5 t6
   = R6_V1 t1
   | R6_V2 t2
   | R6_V3 t3
   | R6_V4 t4
   | R6_V5 t5
   | R6_V6 t6
   deriving (Show, Eq)

data R7 t1 t2 t3 t4 t5 t6 t7
   = R7_V1 t1
   | R7_V2 t2
   | R7_V3 t3
   | R7_V4 t4
   | R7_V5 t5
   | R7_V6 t6
   | R7_V7 t7
   deriving (Show, Eq)

data R8 t1 t2 t3 t4 t5 t6 t7 t8
   = R8_V1 t1
   | R8_V2 t2
   | R8_V3 t3
   | R8_V4 t4
   | R8_V5 t5
   | R8_V6 t6
   | R8_V7 t7
   | R8_V8 t8
   deriving (Show, Eq)

data R9 t1 t2 t3 t4 t5 t6 t7 t8 t9
   = R9_V1 t1
   | R9_V2 t2
   | R9_V3 t3
   | R9_V4 t4
   | R9_V5 t5
   | R9_V6 t6
   | R9_V7 t7
   | R9_V8 t8
   | R9_V9 t9
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

instance TemplateCtxsM ctx (R6 t1 t2 t3 t4 t5 t6) m a
      => WithVariantsM ctx (R6 t1 t2 t3 t4 t5 t6) m a where
    withVariantsM _ f = \case
        R6_V1 a -> f a
        R6_V2 a -> f a
        R6_V3 a -> f a
        R6_V4 a -> f a
        R6_V5 a -> f a
        R6_V6 a -> f a

instance TemplateCtxsM ctx (R7 t1 t2 t3 t4 t5 t6 t7) m a
      => WithVariantsM ctx (R7 t1 t2 t3 t4 t5 t6 t7) m a where
    withVariantsM _ f = \case
        R7_V1 a -> f a
        R7_V2 a -> f a
        R7_V3 a -> f a
        R7_V4 a -> f a
        R7_V5 a -> f a
        R7_V6 a -> f a
        R7_V7 a -> f a

instance TemplateCtxsM ctx (R8 t1 t2 t3 t4 t5 t6 t7 t8) m a
      => WithVariantsM ctx (R8 t1 t2 t3 t4 t5 t6 t7 t8) m a where
    withVariantsM _ f = \case
        R8_V1 a -> f a
        R8_V2 a -> f a
        R8_V3 a -> f a
        R8_V4 a -> f a
        R8_V5 a -> f a
        R8_V6 a -> f a
        R8_V7 a -> f a
        R8_V8 a -> f a

instance TemplateCtxsM ctx (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) m a
      => WithVariantsM ctx (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) m a where
    withVariantsM _ f = \case
        R9_V1 a -> f a
        R9_V2 a -> f a
        R9_V3 a -> f a
        R9_V4 a -> f a
        R9_V5 a -> f a
        R9_V6 a -> f a
        R9_V7 a -> f a
        R9_V8 a -> f a
        R9_V9 a -> f a

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

instance TemplateCtxs ctx (R6 t1 t2 t3 t4 t5 t6) a
      => WithVariants ctx (R6 t1 t2 t3 t4 t5 t6) a where
    withVariants _ f = \case
        R6_V1 a -> f a
        R6_V2 a -> f a
        R6_V3 a -> f a
        R6_V4 a -> f a
        R6_V5 a -> f a
        R6_V6 a -> f a

instance TemplateCtxs ctx (R7 t1 t2 t3 t4 t5 t6 t7) a
      => WithVariants ctx (R7 t1 t2 t3 t4 t5 t6 t7) a where
    withVariants _ f = \case
        R7_V1 a -> f a
        R7_V2 a -> f a
        R7_V3 a -> f a
        R7_V4 a -> f a
        R7_V5 a -> f a
        R7_V6 a -> f a
        R7_V7 a -> f a

instance TemplateCtxs ctx (R8 t1 t2 t3 t4 t5 t6 t7 t8) a
      => WithVariants ctx (R8 t1 t2 t3 t4 t5 t6 t7 t8) a where
    withVariants _ f = \case
        R8_V1 a -> f a
        R8_V2 a -> f a
        R8_V3 a -> f a
        R8_V4 a -> f a
        R8_V5 a -> f a
        R8_V6 a -> f a
        R8_V7 a -> f a
        R8_V8 a -> f a

instance TemplateCtxs ctx (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) a
      => WithVariants ctx (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) a where
    withVariants _ f = \case
        R9_V1 a -> f a
        R9_V2 a -> f a
        R9_V3 a -> f a
        R9_V4 a -> f a
        R9_V5 a -> f a
        R9_V6 a -> f a
        R9_V7 a -> f a
        R9_V8 a -> f a
        R9_V9 a -> f a




-- MkRecord templates
type family RecordTemplate (variants :: [*]) :: *
--type instance RecordTemplate '[] = R0
type instance RecordTemplate '[t1] = R1 t1
type instance RecordTemplate '[t1,t2] = R2 t1 t2
type instance RecordTemplate '[t1,t2,t3] = R3 t1 t2 t3
type instance RecordTemplate '[t1,t2,t3,t4] = R4 t1 t2 t3 t4
type instance RecordTemplate '[t1,t2,t3,t4,t5] = R5 t1 t2 t3 t4 t5
type instance RecordTemplate '[t1,t2,t3,t4,t5,t6] = R6 t1 t2 t3 t4 t5 t6
type instance RecordTemplate '[t1,t2,t3,t4,t5,t6,t7] = R7 t1 t2 t3 t4 t5 t6 t7
type instance RecordTemplate '[t1,t2,t3,t4,t5,t6,t7,t8] = R8 t1 t2 t3 t4 t5 t6 t7 t8
type instance RecordTemplate '[t1,t2,t3,t4,t5,t6,t7,t8,t9] = R9 t1 t2 t3 t4 t5 t6 t7 t8 t9


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

instance ConsByIdx 0 t1 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V1
instance ConsByIdx 1 t2 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V2
instance ConsByIdx 2 t3 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V3
instance ConsByIdx 3 t4 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V4
instance ConsByIdx 4 t5 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V5
instance ConsByIdx 5 t6 (R6 t1 t2 t3 t4 t5 t6) where consByIdx _ = R6_V6

instance ConsByIdx 0 t1 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V1
instance ConsByIdx 1 t2 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V2
instance ConsByIdx 2 t3 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V3
instance ConsByIdx 3 t4 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V4
instance ConsByIdx 4 t5 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V5
instance ConsByIdx 5 t6 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V6
instance ConsByIdx 6 t7 (R7 t1 t2 t3 t4 t5 t6 t7) where consByIdx _ = R7_V7

instance ConsByIdx 0 t1 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V1
instance ConsByIdx 1 t2 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V2
instance ConsByIdx 2 t3 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V3
instance ConsByIdx 3 t4 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V4
instance ConsByIdx 4 t5 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V5
instance ConsByIdx 5 t6 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V6
instance ConsByIdx 6 t7 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V7
instance ConsByIdx 7 t8 (R8 t1 t2 t3 t4 t5 t6 t7 t8) where consByIdx _ = R8_V8

instance ConsByIdx 0 t1 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V1
instance ConsByIdx 1 t2 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V2
instance ConsByIdx 2 t3 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V3
instance ConsByIdx 3 t4 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V4
instance ConsByIdx 4 t5 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V5
instance ConsByIdx 5 t6 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V6
instance ConsByIdx 6 t7 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V7
instance ConsByIdx 7 t8 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V8
instance ConsByIdx 8 t9 (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where consByIdx _ = R9_V9

-- Variants
type instance Variants (R1 t1) = '[t1]
type instance Variants (R2 t1 t2) = '[t1,t2]
type instance Variants (R3 t1 t2 t3) = '[t1,t2,t3]
type instance Variants (R4 t1 t2 t3 t4) = '[t1,t2,t3,t4]
type instance Variants (R5 t1 t2 t3 t4 t5) = '[t1,t2,t3,t4,t5]
type instance Variants (R6 t1 t2 t3 t4 t5 t6) = '[t1,t2,t3,t4,t5,t6]
type instance Variants (R7 t1 t2 t3 t4 t5 t6 t7) = '[t1,t2,t3,t4,t5,t6,t7]
type instance Variants (R8 t1 t2 t3 t4 t5 t6 t7 t8) = '[t1,t2,t3,t4,t5,t6,t7,t8]
type instance Variants (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) = '[t1,t2,t3,t4,t5,t6,t7,t8,t9]

-- Reprs

instance (Repr t1) => Repr (R1 t1) where repr (R1_V1 t) = repr t

instance (Repr t1, Repr t2) => Repr (R2 t1 t2) where
    repr = \case
        R2_V1 t -> repr t
        R2_V2 t -> repr t

instance (Repr t1, Repr t2, Repr t3) => Repr (R3 t1 t2 t3) where
    repr = \case
        R3_V1 t -> repr t
        R3_V2 t -> repr t
        R3_V3 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4) => Repr (R4 t1 t2 t3 t4) where
    repr = \case
        R4_V1 t -> repr t
        R4_V2 t -> repr t
        R4_V3 t -> repr t
        R4_V4 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5) => Repr (R5 t1 t2 t3 t4 t5) where
    repr = \case
        R5_V1 t -> repr t
        R5_V2 t -> repr t
        R5_V3 t -> repr t
        R5_V4 t -> repr t
        R5_V5 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5, Repr t6) => Repr (R6 t1 t2 t3 t4 t5 t6) where
    repr = \case
        R6_V1 t -> repr t
        R6_V2 t -> repr t
        R6_V3 t -> repr t
        R6_V4 t -> repr t
        R6_V5 t -> repr t
        R6_V6 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5, Repr t6, Repr t7) => Repr (R7 t1 t2 t3 t4 t5 t6 t7) where
    repr = \case
        R7_V1 t -> repr t
        R7_V2 t -> repr t
        R7_V3 t -> repr t
        R7_V4 t -> repr t
        R7_V5 t -> repr t
        R7_V6 t -> repr t
        R7_V7 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5, Repr t6, Repr t7, Repr t8) => Repr (R8 t1 t2 t3 t4 t5 t6 t7 t8) where
    repr = \case
        R8_V1 t -> repr t
        R8_V2 t -> repr t
        R8_V3 t -> repr t
        R8_V4 t -> repr t
        R8_V5 t -> repr t
        R8_V6 t -> repr t
        R8_V7 t -> repr t
        R8_V8 t -> repr t

instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5, Repr t6, Repr t7, Repr t8, Repr t9) => Repr (R9 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
    repr = \case
        R9_V1 t -> repr t
        R9_V2 t -> repr t
        R9_V3 t -> repr t
        R9_V4 t -> repr t
        R9_V5 t -> repr t
        R9_V6 t -> repr t
        R9_V7 t -> repr t
        R9_V8 t -> repr t
        R9_V9 t -> repr t

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

--data A = A Int deriving (Show, Eq, Typeable)
--data B = B Int deriving (Show, Eq, Typeable)
--data C = C Int deriving (Show, Eq, Typeable)

----type instance NatRep A = [1,2,3]
----type instance NatRep B = [1,2,4]
----type instance NatRep C = [1,3,2]

--type Foox = Record '[A]


--type Foo1 = Record '[A]
--type Foo2 = Record '[A,B]
--type Foo3 = Record '[A,B,C]

--type Foo2' = Record '[B,A]

--data X = X { _xrec :: Foo1 } deriving (Show)

--makeLenses ''X

--instance IsVariant X where
--    variant = X
--    record  = xrec

--type instance Variants X = '[A]

--ofType :: a -> a -> a
--ofType = const

--test = do
--    let x1 = cons (A 1) :: Foo1
--        x2 = cons (B 1) :: Foo2
--        x3 = cons (C 1) :: Foo3
--        x4 = cons (A 1) :: Maybe Foo3
--        x5 = cons (C 1) :: Maybe Foo3
--        x6 = cons (C 1) :: Maybe Foo1

--        t1 = cons (A 1) :: X
--    --    --x7 = cons (B 1) :: Foo1


--    print $ x1
--    print $ x2
--    print $ x3
--    print $ x4
--    print $ x5
--    print $ x6
--    print "---"

--    print $ t1
--    print "---"

--    let y1 = cast x1 :: Foo1
--        y2 = cast x2 :: Foo3
--        y3 = cast x2 :: Maybe Foo1
--        --y4 = cast x2 :: Foo1

--    print $ y1
--    print $ y2
--    print $ y3
--    print "---"

--    let x1'  = specificCons (A 1) :: Foo1
--        x1'' = wrappedCons (A 1) :: Found Foo1
--        x2'' = wrappedCons (B 1) :: NotFound Foo1


--    let tst = cons (B 1) :: Foo2
--        --tst' = tst :: Foo2'

--    print $ case' tst $ do
--        match $ \(A a) -> "a"
--        --match $ \(B a) -> "b"
--        match $ \ANY   -> "x"

--    print $ case' t1 $ do
--        match $ \(A a) -> "a"
--        match $ \(B a) -> "b"
--        match $ \ANY   -> "x"
--    print "==="
--    putStrLn ""


--main = test