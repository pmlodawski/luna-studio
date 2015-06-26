

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

--------------------------

import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)


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

---

type family Remove a (set :: k) :: k
type instance Remove v (a ': b) = If (v :== a) b (a ': Remove v b)
type instance Remove v (a,b)    = If (v :== a) b (a, Remove v b)

---

type family Diff (set :: k) (set' :: k) :: k
type instance Diff (a ': b) set = Diff b (Remove a set)
type instance Diff '[] set = set

---

type family In a (set :: k) :: Bool
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


------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

data Record a where
    Record :: IsSet a => (RecordTemplate (ToSet a)) -> Record a

fromRecord :: Record a -> RecordTemplate a
fromRecord (Record a) = a

type family NatRep (a :: k) :: [Nat]
type family MkRecord variants where
    MkRecord variants = Record (ToSet variants)

type VariantCons r t = ConsByIdx (Index (NatRep t) (VariantReprsOf (VariantsOf r))) t r

-- === Utils ===

cons :: forall a cls idx. (idx ~ Index (NatRep a) (VariantReprsOf cls), ConsByIdx idx a cls)
     => a -> cls
cons = consByIdx (Proxy :: Proxy idx)

-- === Instances ===

instance (IsSet a, VariantShow (RecordTemplate a)) => Show (Record a) where
    showsPrec d (Record a) = showParen (d > app_prec) $ showString $ "Record (" <> variantShow a <> ")"
         where app_prec = 10


------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type family VariantsOf a     :: [*]
type family VariantReprsOf (a :: k) :: [[Nat]] where
    VariantReprsOf (v ': vs) = NatRep v ': VariantReprsOf vs
    VariantReprsOf a         = VariantReprsOf (VariantsOf a)

type instance VariantsOf (Maybe a) = VariantsOf a

type instance VariantsOf (Record vars) = vars

--type instance VariantReprsOf a = VariantReprsOf (VariantsOF a)
--type instance VariantReprsOf (v ': vs) = NatRep v ': VariantReprsOf vs

class ConsByIdx (idx :: Maybe Nat) a cls where
    consByIdx :: Proxy idx -> a -> cls

-- === ConsByIdx Instances ===

-- Basic handlers

instance (ConsByIdx (Just n) a (RecordTemplate r), IsSet r) => ConsByIdx (Just n) a (Record r) where
    consByIdx = Record .: consByIdx

-- Maybe support

instance ConsByIdx Nothing a (Maybe r) where
    consByIdx _ _ = Nothing

instance ConsByIdx (Just n) a r => ConsByIdx (Just n) a (Maybe r) where
    consByIdx = Just .: consByIdx

-- Failure assertion

class InvalidRecordType a b | a -> b
instance InvalidRecordType r a => ConsByIdx Nothing a r where consByIdx = undefined


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
match m = withState (<> [Match $ withVariant m])

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

instance (Cast (RecordTemplate vars) r, IsSet vars) => Cast (Record vars) r where
    cast (Record a) = cast a

instance (RecordCast a r) => Cast a r where
    cast = templateCast


------------------------------------------------------------------------
-- Record bulk processing
------------------------------------------------------------------------

class RecordProc tp r out | tp r -> out where
    recordProc :: tp -> r -> out

class VariantProc tp a b | tp a -> b where
    variantProc :: tp -> a -> b

class RecordPolyProc tp r out where
    recordPolyProc :: tp -> r -> out

class RecordPolyProcM tp r m out | tp r -> m where
    recordPolyProcM :: tp -> r -> m out

class VariantPolyProc tp a b where
    variantPolyProc :: tp -> a -> b

class VariantPolyProcM tp a m b | tp a -> m where
    variantPolyProcM :: tp -> a -> m b


instance (IsSet vars, RecordProc tp (RecordTemplate vars) out) => RecordProc tp (Record vars) out where
    recordProc tp (Record a) = recordProc tp a

instance (IsSet vars, RecordPolyProc tp (RecordTemplate vars) out) => RecordPolyProc tp (Record vars) out where
    recordPolyProc tp (Record a) = recordPolyProc tp a

instance (IsSet vars, RecordPolyProcM tp (RecordTemplate vars) m out) => RecordPolyProcM tp (Record vars) m out where
    recordPolyProcM tp (Record a) = recordPolyProcM tp a


-- === VariantShow ===

data VariantRawShow = VariantRawShow
type VariantShow a   = RecordProc VariantRawShow a String

instance Show a => VariantProc VariantRawShow a String where
    variantProc _ = show

variantShow :: VariantShow a => a -> String
variantShow   = recordProc VariantRawShow

-- === Record Template casting ===

data VariantCast = VariantCast
type RecordCast a r = RecordPolyProc VariantCast a r

instance VariantCons r a => VariantPolyProc VariantCast a r where
    variantPolyProc _ = cons

templateCast :: RecordCast a b => a -> b
templateCast = recordPolyProc VariantCast

-- === Record Bulk modification ===

class AppByType a b where
    appByType :: (a -> out) -> b -> Maybe out

instance AppByType a a where
    appByType = Just .: ($)

instance AppByType a b where
    appByType _ _ = Nothing

---

type RecordMod a rec out = RecordProc (VariantMod a out) rec (Maybe out)
data VariantMod a b = VariantMod (a -> b)

instance AppByType a b => VariantProc (VariantMod a out) b (Maybe out) where
    variantProc (VariantMod f) = appByType f

withVariant :: RecordMod a rec out => (a -> out) -> rec -> Maybe out
withVariant f r = recordProc (VariantMod f) r

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

-- Variant Proc

instance (VariantProc tp t1 out)
      => RecordProc tp (R1 t1) out where
    recordProc tp = \case
        R1_V1 a -> variantProc tp a

instance (VariantProc tp t1 out, VariantProc tp t2 out)
      => RecordProc tp (R2 t1 t2) out where
    recordProc tp = \case
        R2_V1 a -> variantProc tp a
        R2_V2 a -> variantProc tp a

instance (VariantProc tp t1 out, VariantProc tp t2 out, VariantProc tp t3 out)
      => RecordProc tp (R3 t1 t2 t3) out where
    recordProc tp = \case
        R3_V1 a -> variantProc tp a
        R3_V2 a -> variantProc tp a
        R3_V3 a -> variantProc tp a

---

instance (VariantPolyProc tp t1 out)
      => RecordPolyProc tp (R1 t1) out where
    recordPolyProc tp = \case
        R1_V1 a -> variantPolyProc tp a

instance (VariantPolyProc tp t1 out, VariantPolyProc tp t2 out)
      => RecordPolyProc tp (R2 t1 t2) out where
    recordPolyProc tp = \case
        R2_V1 a -> variantPolyProc tp a
        R2_V2 a -> variantPolyProc tp a

instance (VariantPolyProc tp t1 out, VariantPolyProc tp t2 out, VariantPolyProc tp t3 out)
      => RecordPolyProc tp (R3 t1 t2 t3) out where
    recordPolyProc tp = \case
        R3_V1 a -> variantPolyProc tp a
        R3_V2 a -> variantPolyProc tp a
        R3_V3 a -> variantPolyProc tp a

---

instance (VariantPolyProcM tp t1 m out)
      => RecordPolyProcM tp (R1 t1) m out where
    recordPolyProcM tp = \case
        R1_V1 a -> variantPolyProcM tp a

instance (VariantPolyProcM tp t1 m out, VariantPolyProcM tp t2 m out)
      => RecordPolyProcM tp (R2 t1 t2) m out where
    recordPolyProcM tp = \case
        R2_V1 a -> variantPolyProcM tp a
        R2_V2 a -> variantPolyProcM tp a

instance (VariantPolyProcM tp t1 m out, VariantPolyProcM tp t2 m out, VariantPolyProcM tp t3 m out)
      => RecordPolyProcM tp (R3 t1 t2 t3) m out where
    recordPolyProcM tp = \case
        R3_V1 a -> variantPolyProcM tp a
        R3_V2 a -> variantPolyProcM tp a
        R3_V3 a -> variantPolyProcM tp a



--class VariantPolyProc tp a b where
--    variantPolyProc :: tp -> a -> b

--class VariantPolyProcM tp a m b | tp a -> m where
--    variantPolyProcM :: tp -> a -> m b

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
type instance VariantsOf (R1 t1) = '[t1]
type instance VariantsOf (R2 t1 t2) = '[t1,t2]
type instance VariantsOf (R3 t1 t2 t3) = '[t1,t2,t3]

-- VariantReprsOf
--type instance VariantReprsOf (Maybe a)     = VariantReprsOf a
--type instance VariantReprsOf (R1 t1)       = '[NatRep t1]
--type instance VariantReprsOf (R2 t1 t2)    = '[NatRep t1, NatRep t2]
--type instance VariantReprsOf (R3 t1 t2 t3) = '[NatRep t1, NatRep t2, NatRep t3]






------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

data A = A Int deriving (Show, Typeable)
data B = B Int deriving (Show, Typeable)
data C = C Int deriving (Show, Typeable)

type instance NatRep A = [1,2,3]
type instance NatRep B = [1,2,4]
type instance NatRep C = [1,3,2]

type Foox = Record '[A]


type Foo1 = MkRecord '[A]
type Foo2 = MkRecord '[A,B]
type Foo3 = MkRecord '[A,B,C]

type Foo2' = MkRecord '[B,A]



main_old = do
    let x1 = cons (A 1) :: Foo1
        x2 = cons (B 1) :: Foo2
        x3 = cons (C 1) :: Foo3
        x4 = cons (C 1) :: Maybe Foo3

        xx = cons (A 1) :: Foox

        y1 = cast x1 :: Foo1
        y2 = cast x2 :: Maybe Foo3
        y3 = cast x2 :: Foo3

    print $ x1
    print $ x2
    print $ x3
    print $ x4

    print "---"
    print $ y1
    print $ y3
    print $ y2

    print "---"
    let tst = cons (A 1) :: Foo2
        tst' = tst :: Foo2'

    print $ vcase tst $ do
        match $ \(A a) -> "a"
        match $ \(B a) -> "b"
    print "==="
    putStrLn ""


-------------------------------------------------------------------------------------------------------------------

type Variants vars = (IsSet vars, RecordProc VariantRawShow (RecordTemplate vars) String)
------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------

data Error = UnknownSpecial String
           | UnknownVar     String
           | NegativePath
           deriving (Show)


data Segment segs = Root
                  | Node String
                  | Segment (Record segs)

--deriving instance Variants segs => Show (Segment segs)
instance Variants segs => Show (Segment segs) where
    showsPrec d = \case
        Root               -> showString "Root"
        Node    s          -> showParen (d > app_prec) $ showString "Node " . showsPrec (app_prec+1) s
        Segment (Record r) -> showParen (d > app_prec) $ showString $ "Segment (" <> variantShow r <> ")"
        where app_prec = 10

newtype Path (segs :: [*]) = Path [Segment segs] deriving (Typeable)

deriving instance Variants segs => Show (Path segs)

---




type RelPath = Path '[Var]


nonEmpty = filter (/= mempty)


readPath :: RecordReader segs => String -> Either Error (Path segs)
readPath str@(s:_)  = fmap (Path . mount . concat) . sequence $ fmap (Right  . readPathSegment) segs where
    mount     = if s == '/' then (Root:) else id
    segs      = nonEmpty $ splitOn "/" str
    --readSeg s = if and $ fmap (=='.') s
--        then Right $ replicate (length s - 1) Up
--        else fmap return $ case s of
--            --"~"    -> Right home
--            (a:as) -> case a of
--                '$' -> Right $ Var as
--                --'@' -> readSpecial as
--                _   -> Right . Segment $ Node s

--class SegmentsReader s where
--    readSegments :: String -> Maybe (Record s)

--instance SegmentsReader segs => SegmentsReader (seg ': segs) where
--    readSegments s = case (readSegment s :: Maybe seg) of
--        Just x -> Just $ cons x
--        _      -> readSegments

--type VariantCons r t = ConsByIdx (Index (NatRep t) (VariantReprsOf r)) t r

---

data VariantSegmentShow = VariantSegmentShow
type SegmentShowCtx a   = RecordProc VariantSegmentShow (RecordTemplate a) String

instance PathSegment a => VariantProc VariantSegmentShow a String where
    variantProc _ = showSegment

---


showPath :: (IsSet segs, SegmentShowCtx segs) => Path segs -> String
showPath (Path segs) = intercalate "/" $ fmap showSeg segs where
    showSeg = \case
        Root      -> "/"
        Node    s -> s
        Segment s -> recordProc VariantSegmentShow s

type RecordReader s = RecordReader' s s

class RecordReader' (s :: [*]) rs where
    readRecord :: Proxy s -> String -> Maybe [Record rs]

instance (VariantCons (Record r) seg, PathSegment seg, RecordReader' segs r) => RecordReader' (seg ': segs) r where
    readRecord _ s = case (readSegment s :: Maybe [seg]) of
        Just x  -> Just $ fmap cons x
        Nothing -> readRecord (Proxy :: Proxy segs) s

instance RecordReader' '[] r where
    readRecord _ _ = Nothing

class PathSegment s where
    readSegment :: String -> Maybe [s]
    showSegment :: s -> String



readPathSegment :: forall (segs :: [*]). RecordReader segs => String -> [Segment segs]
readPathSegment s = case readRecord (Proxy :: Proxy segs) s of
    Just s  -> fmap Segment s
    Nothing -> [Node s]


---

data Up = Up deriving (Show)
type instance NatRep Up = '[0]

instance PathSegment Up where
    readSegment s = if and $ fmap (=='.') s
        then Just $ replicate (length s - 1) Up
        else Nothing
    showSegment _ = ".."

instance SegmentExpander Up IO where
    expandSegment segs _ = do
        return $ tail segs

---

data Var = Var String deriving (Show, Typeable)
type instance NatRep Var = '[1]

instance PathSegment Var where
    readSegment = \case
        ('$':s) -> Just [Var s]
        _       -> Nothing
    showSegment (Var s) = '$' : s

---

data Temp = Temp Locality Persistence
             deriving (Show, Typeable)

data Locality = Global
              | Local
              deriving (Show, Generic)

data Persistence = Persistent
                 | NonPersistent
                 deriving (Show, Typeable)


type instance NatRep Temp = '[2]

instance PathSegment Temp where
    readSegment = readSpecial "temp" [Temp Local NonPersistent]
    showSegment _ = showSpecial "temp"

readSpecial s val = \case
    ('@' : str) -> if s == str then Just val
                               else Nothing
    _           -> Nothing

showSpecial = ('@':)

---

--class PathExpander p p' where
--    expandPath :: Path p -> Path p'

--instance (diff ~ Diff p' p) => PathExpander p p' where
--    expandPath = expandSegments (Proxy :: Proxy diff)

--class SegmentsExpander (segs :: [*]) p p' | segs p -> p' where
--    expandSegments :: Proxy segs -> Path p -> Path p'

--c

--Record (RecordTemplate (ToSet a))

    --data VariantCast = VariantCast
    --type RecordCast a r = RecordPolyProc VariantCast a r

    --instance VariantCons r a => VariantPolyProc VariantCast a r where
    --    variantPolyProc _ = cons

    --templateCast :: RecordCast a b => a -> b
    --templateCast = recordPolyProc VariantCast


type SegmentExpandCtx p m p' = ( Monad m, Functor m, IsSet p
                               , RecordPolyProcM (VariantSegmentExpand (Segment p') (Diff p' p)) (RecordTemplate p) m [Segment p']
                               )
data VariantSegmentExpand r (segs :: [*]) = VariantSegmentExpand { unXXX :: [r] }

instance (selected ~ In a segs, SegmentCondExpander selected a m (Segment r))
      => VariantPolyProcM (VariantSegmentExpand (Segment r) segs) a m [Segment r] where
    variantPolyProcM (VariantSegmentExpand ss) = expandSegmentCond (Proxy :: Proxy selected) ss

class SegmentCondExpander (cond :: Bool) a m r | a -> m where
    expandSegmentCond :: Proxy cond -> [r] -> a -> m [r]

instance VariantCons (Record r) a => SegmentCondExpander False a IO (Segment r) where
    expandSegmentCond _ ss s = return $ ss <> [Segment $ cons s]

instance (SegmentExpander a m) => SegmentCondExpander True a m (Segment r) where
    expandSegmentCond _ = expandSegment




class SegmentExpander s m | s -> m where
    expandSegment :: [Segment segs] -> s -> m [Segment segs]



expandPathSegment :: forall segDiff t segs m.
                 (Monad m, RecordPolyProcM (VariantSegmentExpand (Segment segs) segDiff) (RecordTemplate t) m [Segment segs], IsSet t)
              => Proxy segDiff -> [Segment segs] -> Segment t -> m [Segment segs]
expandPathSegment segSelection expSegs = \case
    Root      -> return $ expSegs <> [Root]
    Node    s -> return $ expSegs <> [Node s]
    Segment s -> recordPolyProcM (vse expSegs) s
    where vse a = VariantSegmentExpand a :: VariantSegmentExpand (Segment segs) segDiff


expandPath :: forall p m p'. (SegmentExpandCtx p m p') => Path p -> m (Path p')
expandPath (Path segs) = fmap Path $ foldM (expandPathSegment (Proxy :: Proxy (Diff p' p))) [] segs

type SegmentExpandSingleCtx v = (p' ~ Remove v p, SegmentExpandCtx p m p') => Path p -> m (Path p')

expandUp :: SegmentExpandSingleCtx Up
expandUp = expandPath

expandVar :: SegmentExpandSingleCtx Var
expandVar = expandPath


---


deriving instance Typeable '[]
deriving instance Typeable '(:)

main = do
    main_old

    let --p = var "foo" :: Segment '[Var]
        p2 = readPath "@temp/bar/$foo/.." :: (Either Error (Path '[Up, Var, Temp]))

    --print p
    print p2

    case p2 of
        Left _  -> return ()
        Right p -> do
            print $ showPath p
            (print . showPath) =<< expandVar p
            return ()

    --let f = Foo (R1_V1 (1::Int)) :: Foo '[Int]

    print "end"

