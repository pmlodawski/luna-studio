

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

{-# LANGUAGE DysfunctionalDependencies #-}

module Flowbox.System.Variants where

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
import           Flowbox.System.Types
import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)
import           GHC.Exts

#include "ghcplatform.h"



------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

--data Record variants = Record (RecordTemplate variants) deriving (Eq)

data Record a where
    Record :: Eq (RecordTemplate a) => (RecordTemplate a) -> Record a

deriving instance Eq (Record a)

fromRecord :: Record a -> RecordTemplate a
fromRecord (Record a) = a


type family MkRecord variants where
    MkRecord variants = Record (ToSet variants)

-- === Utils ===

type family VariantsOf a :: [*]
type family VariantReprsOf (a :: [*]) :: [[Nat]] where
    VariantReprsOf (v ': vs) = NatRep v ': VariantReprsOf vs
    VariantReprsOf '[]       = '[]
    --VariantReprsOf a         = VariantReprsOf (VariantsOf a)

type instance VariantsOf (Maybe a) = VariantsOf a
type instance VariantsOf (Record vars) = vars

-- === Instances ===

instance (IsSet a, VariantShow (RecordTemplate a)) => Show (Record a) where
    showsPrec d (Record a) = showParen (d > app_prec) $ showString $ "Record (" <> variantShow a <> ")"
         where app_prec = 10


------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type VariantPolyCons a vType    = ConsByIdx (VariantIdx a vType) vType a
type VariantPolyCons2 vType m cls    = ConsByIdx2 (VariantIdx cls vType) vType m cls
type VariantCons     vars vType = VariantPolyCons (Record vars) vType
type VariantIdx      vars vType = Index (NatRep vType) (VariantReprsOf (VariantsOf vars))


-- === Variant constructors ===

data Val a = Val { fromVal :: a } deriving (Show)
data NoVal a = NoVal deriving (Show)

class ConsByIdx2 (idx :: Maybe Nat) a m cls | idx -> m where
    consByIdx2 :: Proxy idx -> a -> m cls

class ConsByIdx3 (idx :: Nat) a cls where
    consByIdx3 :: Proxy idx -> a -> cls


instance ConsByIdx2 Nothing a NoVal cls where
    consByIdx2 _ _ = NoVal

instance ConsByIdx3 n a cls => ConsByIdx2 ('Just n) a Val cls where
    consByIdx2 _ a = Val $ consByIdx3 (Proxy :: Proxy n) a




class ConsByIdx (idx :: Maybe Nat) a cls where
    consByIdx :: Proxy idx -> a -> cls

cons :: forall vType rec idx. (VariantPolyCons rec vType, idx ~ VariantIdx rec vType)
     => vType -> rec
cons = consByIdx (Proxy :: Proxy idx)

cons2 :: forall vType m cls idx. (idx ~ VariantIdx cls vType, ConsByIdx2 idx vType m cls)
     => vType -> m cls
cons2 = consByIdx2 (Proxy :: Proxy idx)

--cons' ::
cons' = fromVal . cons2

class Constructor vType out where
    cons'' :: vType -> out

class ToMaybe m where
    toMaybe :: m a -> Maybe a

instance ToMaybe Val where
    toMaybe = Just . fromVal

instance ToMaybe NoVal where
    toMaybe _ = Nothing

instance VariantPolyCons2 vType Val cls => Constructor vType cls where
    cons'' = fromVal . cons2

instance (VariantPolyCons2 vType m cls, ToMaybe m) => Constructor vType (Maybe cls) where
    cons'' = toMaybe . cons2

-- Basic handlers

instance (ConsByIdx3 n a (RecordTemplate r), Eq (RecordTemplate r))
      => ConsByIdx3 n a (Record r) where
    consByIdx3 = Record .: consByIdx3

instance (ConsByIdx (Just n) a (RecordTemplate r), IsSet r, Eq (RecordTemplate r)) => ConsByIdx (Just n) a (Record r) where
    consByIdx = Record .: consByIdx

-- Maybe support

instance ConsByIdx Nothing a (Maybe r) where
    consByIdx _ _ = Nothing

instance ConsByIdx (Just n) a r => ConsByIdx (Just n) a (Maybe r) where
    consByIdx = Just .: consByIdx

-- Failure assertion

class InvalidRecordType a b | a -> b
instance InvalidRecordType r a => ConsByIdx Nothing a r where
    consByIdx = undefined

--class InvalidRecordType2 (idx :: Maybe Nat) (m :: * -> *) a cls | idx -> m
--instance InvalidRecordType2 idx m a cls => ConsByIdx2 idx a m cls


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

match :: RecordMod variant rec out => (variant -> out) -> MatchSet rec out
match m = withState (<> [Match $ mapVariant m])

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

data CAST = CAST
type Cast a b = WithVariants CAST a b

cast :: Cast a b => a -> b
cast = withVariants CAST

instance (VariantPolyCons r a, Monad m) => WithVariantM CAST a m r where
    withVariantM _ = return . cons


--data SECURE_CAST = SECURE_CAST
--type SecureCast a b = WithVariants SECURE_CAST a b

--class WithVariantsX2 ctx (vars :: [*]) rec m rec' | rec -> vars where
--    withVariantsX2 :: Proxy ctx -> (forall x n y. ctx vars x n y => Proxy vars -> x -> n y) -> rec -> m rec'

--class WithVariantX2 ctx vars a m b where
--    withVariantX2 :: ctx vars a m b => Proxy ctx -> (forall x n y. ctx vars x n y => Proxy vars -> x -> n y) -> a -> m b

        --instance WithVariantX2 ctx a m b where
        --    withVariantX2 _ f a = f a

class Cast2 (vars :: [*]) a m b | a b -> m where
    cast2 :: Proxy vars -> a -> m b

instance (VariantPolyCons2 a m b) => Cast2 vars a m b where
    cast2 _ = cons2

--cast2' :: WithVariantsX2 Cast2 a m b => a -> m b
cast2' = withVariantsX2 (Proxy :: Proxy Cast2) cast2



--type Cast a b = WithVariants CAST a b

--cast :: Cast a b => a -> b
--cast = withVariants CAST

--cons2 :: forall vType m cls idx. (idx ~ VariantIdx cls vType, ConsByIdx2 idx vType m cls)
--     => vType -> m cls
--cons2 = consByIdx2 (Proxy :: Proxy idx)

--instance (VariantPolyCons2 a m cls) => WithVariantM' SECURE_CAST a m cls where
--    withVariantM' _ = cons2

--cast' = withVariants' SECURE_CAST

--class Cast2 a b where
--    cast2 :: a -> b

--instance SecureCast a (Val b) => Cast2 a b where
--    cast2 = fromVal . cast'

--instance (SecureCast a (m b), ToMaybe m) => Cast2 a (Maybe b) where
--    cast2 = toMaybe . cast'

--xxx = toMaybe . cast'

------------------------------------------------------------------------
-- Record bulk processing
------------------------------------------------------------------------

--class Foo a where
--    foo :: a -> String

--instance Show a => Foo a where
--    foo = show

--class Bar ctx where
--    bar :: (ctx a, ctx b) => Proxy ctx -> (forall x. ctx x => x -> String) -> (a,b) -> String

----class Bar2 where
--bar2 :: (forall x. x -> String) -> (a,b) -> String
--bar2 f (a,b) = f a ++ f b

--instance Bar ctx where
--    bar _ f (a,b) = f a ++ f b

--x = bar (Proxy :: Proxy Foo) foo (1, "ala")
--x2 = bar2 foo (1, "ala")

--test = do
--    return ()


--type WithVariants' tp a b = WithVariantsM' tp a Identity b


--class WithVariantsX tp ctx a b where
--    withVariantsX :: ctx a b => Proxy ctx -> tp -> a -> b

--class WithVariantX tp ctx a b where
--    withVariantX :: ctx a b => Proxy ctx -> tp -> a -> b


--type WithVariant' tp a b = WithVariantM' tp a Identity b

--class WithVariantM' tp a m b | tp a -> b where
--    withVariantM' :: tp -> a -> m b

--withVariant' = runIdentity .: withVariantM'

---

type WithVariants' tp a b = WithVariantsM' tp a Identity b

class WithVariantsM' tp a m b | tp a -> b where
    withVariantsM' :: tp -> a -> m b

withVariants' = runIdentity .: withVariantsM'


type WithVariant' tp a b = WithVariantM' tp a Identity b

class WithVariantM' tp a m b | tp a -> b where
    withVariantM' :: tp -> a -> m b

withVariant' = runIdentity .: withVariantM'

---

type WithVariants tp a b = WithVariantsM tp a Identity b

class WithVariantsM tp r m out where
    withVariantsM :: tp -> r -> m out

withVariants :: WithVariants tp a b => tp -> a -> b
withVariants = runIdentity .: withVariantsM


type WithVariant tp a b = WithVariantM tp a Identity b

class WithVariantM tp a m b where
    withVariantM :: tp -> a -> m b

variantPolyProc :: WithVariant tp a b => tp -> a -> b
variantPolyProc = runIdentity .: withVariantM

--

instance (IsSet vars, WithVariantsM tp (RecordTemplate vars) m out) => WithVariantsM tp (Record vars) m out where
    withVariantsM tp (Record a) = withVariantsM tp a

instance (IsSet vars, WithVariantsM' tp (RecordTemplate vars) m out) => WithVariantsM' tp (Record vars) m out where
    withVariantsM' tp (Record a) = withVariantsM' tp a

instance (WithVariantsX2 ctx (RecordTemplate vars) m out, vars ~ VariantsOf (RecordTemplate vars))
      => WithVariantsX2 ctx (Record vars) m out where
    withVariantsX2 ctx f (Record a) = withVariantsX2 ctx f a



--class WithVariantsX2 ctx rec m rec' where
--    withVariantsX2 :: (vars ~ VariantsOf rec)
--                   => Proxy ctx -> (forall x n y. ctx vars x n y => Proxy vars -> x -> n y) -> rec -> m rec'

------------------------------------------------------------------------
-- Variant Utilities
------------------------------------------------------------------------

-- === VariantShow ===

data VariantRawShow = VariantRawShow
type VariantShow a   = WithVariants' VariantRawShow a String

instance (Show a, Monad m) => WithVariantM' VariantRawShow a m String where
    withVariantM' _ = return . show

variantShow :: VariantShow a => a -> String
variantShow   = withVariants' VariantRawShow


-- === Variant map ===

class MaybeApp a b where
    maybeApp :: (a -> out) -> b -> Maybe out

instance MaybeApp a a where
    maybeApp = Just .: ($)

instance MaybeApp a b where
    maybeApp _ _ = Nothing

---

type RecordMod a rec out = WithVariants' (VariantMod a out) rec (Maybe out)
newtype VariantMod a b = VariantMod (a -> b)

instance (MaybeApp a b, Monad m) => WithVariantM' (VariantMod a out) b m (Maybe out) where
    withVariantM' (VariantMod f) = return . maybeApp f

mapVariant :: RecordMod a rec out => (a -> out) -> rec -> Maybe out
mapVariant f r = withVariants' (VariantMod f) r

------------------------------------------------------------------------
-- MkRecord templates
------------------------------------------------------------------------

data R1 a = R1_V1 a
    deriving (Show, Eq)

data R2 a b
    = R2_V1 a
    | R2_V2 b
    deriving (Show, Eq)

data R3 a b c
    = R3_V1 a
    | R3_V2 b
    | R3_V3 c
    deriving (Show, Eq)


-- === Instances ===


class WithVariantsX2 ctx rec m rec' where
    withVariantsX2 :: (vars ~ VariantsOf rec)
                   => Proxy ctx -> (forall x n y. ctx vars x n y => Proxy vars -> x -> n y) -> rec -> m rec'

class WithVariantX2 ctx (vars :: [*]) a m b where
    withVariantX2 :: ctx vars a m b => Proxy ctx -> Proxy vars -> (forall x n y. ctx vars x n y => Proxy vars -> x -> n y) -> a -> m b

instance WithVariantX2 ctx vars a m b where
     withVariantX2 _ v f a = f v a

type XC2 ctx vars t m a = (WithVariantX2 ctx vars t m a, ctx vars t m a)


instance XC2 ctx '[t1] t1 m a => WithVariantsX2 ctx (R1 t1) m a where
    withVariantsX2 ctx f = \case
        R1_V1 a -> withVariantX2 ctx (Proxy :: Proxy '[t1]) f a

instance (XC2 ctx '[t1,t2] t1 m a, XC2 ctx '[t1,t2] t2 m a) => WithVariantsX2 ctx (R2 t1 t2) m a where
    withVariantsX2 ctx f = \case
        R2_V1 a -> withVariantX2 ctx (Proxy :: Proxy '[t1, t2]) f a
        R2_V2 a -> withVariantX2 ctx (Proxy :: Proxy '[t1, t2]) f a

instance (XC2 ctx '[t1,t2,t3] t1 m a, XC2 ctx '[t1,t2,t3] t2 m a, XC2 ctx '[t1,t2,t3] t3 m a)
      => WithVariantsX2 ctx (R3 t1 t2 t3) m a where
    withVariantsX2 ctx f = \case
        R3_V1 a -> withVariantX2 ctx (Proxy :: Proxy '[t1, t2, t3]) f a
        R3_V2 a -> withVariantX2 ctx (Proxy :: Proxy '[t1, t2, t3]) f a
        R3_V3 a -> withVariantX2 ctx (Proxy :: Proxy '[t1, t2, t3]) f a





class WithVariantsX ctx a m b where
    withVariantsX :: ctx a m b => Proxy ctx -> a -> m b

class WithVariantX ctx a m b where
    withVariantX :: ctx a m b => Proxy ctx -> a -> m b

type XC ctx t m a = (WithVariantX ctx t m a, ctx t m a)

instance (XC ctx t1 m a)
      => WithVariantsX ctx (R1 t1) m a where
    withVariantsX ctx = \case
        R1_V1 a -> withVariantX ctx a

instance (XC ctx t1 m a, XC ctx t2 m a)
      => WithVariantsX ctx (R2 t1 t2) m a where
    withVariantsX ctx = \case
        R2_V1 a -> withVariantX ctx a
        R2_V2 a -> withVariantX ctx a

instance (XC ctx t1 m a, XC ctx t2 m a, XC ctx t3 m a)
      => WithVariantsX ctx (R3 t1 t2 t3) m a where
    withVariantsX ctx = \case
        R3_V1 a -> withVariantX ctx a
        R3_V2 a -> withVariantX ctx a
        R3_V3 a -> withVariantX ctx a

-- Variant Proc

instance (WithVariantM' tp t1 m out)
      => WithVariantsM' tp (R1 t1) m out where
    withVariantsM' tp = \case
        R1_V1 a -> withVariantM' tp a

instance (WithVariantM' tp t1 m out, WithVariantM' tp t2 m out)
      => WithVariantsM' tp (R2 t1 t2) m out where
    withVariantsM' tp = \case
        R2_V1 a -> withVariantM' tp a
        R2_V2 a -> withVariantM' tp a

instance (WithVariantM' tp t1 m out, WithVariantM' tp t2 m out, WithVariantM' tp t3 m out)
      => WithVariantsM' tp (R3 t1 t2 t3) m out where
    withVariantsM' tp = \case
        R3_V1 a -> withVariantM' tp a
        R3_V2 a -> withVariantM' tp a
        R3_V3 a -> withVariantM' tp a

---

instance (WithVariantM tp t1 m out)
      => WithVariantsM tp (R1 t1) m out where
    withVariantsM tp = \case
        R1_V1 a -> withVariantM tp a

instance (WithVariantM tp t1 m out, WithVariantM tp t2 m out)
      => WithVariantsM tp (R2 t1 t2) m out where
    withVariantsM tp = \case
        R2_V1 a -> withVariantM tp a
        R2_V2 a -> withVariantM tp a

instance (WithVariantM tp t1 m out, WithVariantM tp t2 m out, WithVariantM tp t3 m out)
      => WithVariantsM tp (R3 t1 t2 t3) m out where
    withVariantsM tp = \case
        R3_V1 a -> withVariantM tp a
        R3_V2 a -> withVariantM tp a
        R3_V3 a -> withVariantM tp a


-- MkRecord templates
type family RecordTemplate (variants :: [*]) :: *
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

instance ConsByIdx3 0 t1 (R1 t1) where consByIdx3 _ = R1_V1

instance ConsByIdx3 0 t1 (R2 t1 t2) where consByIdx3 _ = R2_V1
instance ConsByIdx3 1 t2 (R2 t1 t2) where consByIdx3 _ = R2_V2

instance ConsByIdx3 0 t1 (R3 t1 t2 t3) where consByIdx3 _ = R3_V1
instance ConsByIdx3 1 t2 (R3 t1 t2 t3) where consByIdx3 _ = R3_V2
instance ConsByIdx3 2 t3 (R3 t1 t2 t3) where consByIdx3 _ = R3_V3

-- VariantsOf
type instance VariantsOf (R1 t1) = '[t1]
type instance VariantsOf (R2 t1 t2) = '[t1,t2]
type instance VariantsOf (R3 t1 t2 t3) = '[t1,t2,t3]



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

main = do
    let x1 = cons'' (A 1) :: Foo1
        x2 = cons'' (B 1) :: Foo2
        x3 = cons'' (C 1) :: Foo3
        x4 = cons'' (C 1) :: Maybe Foo3
        x5 = cons'' (C 1) :: Maybe Foo1

        xx = cons'' (A 1) :: Foox

        y1 = cast x1 :: Foo1
        y2 = cast x2 :: Maybe Foo3
        y3 = cast x2 :: Foo3
        y4 = cast x2 :: Maybe Foo1


        --y1' = cast2' x1 `ofType` (undefined :: Val Foo2)

        --y5 = cast x2 :: Foo1

    let x1' = cons' (A 1) :: Foo1
        x1'' = cons2 (A 1) :: Val Foo1
        x2'' = cons2 (B 1) :: NoVal Foo1

    --print y1'

    print $ x1
    print $ x1'
    print $ x2
    print $ x3
    print $ x4
    print $ x5

    print "---"
    print $ y1
    print $ y2
    print $ y3
    print $ y4

    print "---"
    let tst = cons (A 1) :: Foo2
        tst' = tst :: Foo2'

    print $ vcase tst $ do
        match $ \(A a) -> "a"
        match $ \(B a) -> "b"
    print "==="
    putStrLn ""

