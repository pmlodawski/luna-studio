{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}


module Data.Variants where

import Flowbox.Prelude hiding (cons, Index, cast, Variant, Castable)

import Flowbox.System.Types (ToMaybe, toMaybe, IsSubset, orElse, withState, ToSet, Index, In)

import Data.Typeable          hiding (cast)
import Data.Maybe             (fromJust)
import Control.Monad.State    hiding (withState)
import Type.BaseType
import Data.TypeLevel.Bool    ((:==))

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

impossibleError :: String -> String
impossibleError s = "[library Variants] Impossible happened: " <> s <> ". Please report this as a bug."

emptyRecordError :: String
emptyRecordError = impossibleError "empty Record"

uncheckedVariantError :: String
uncheckedVariantError = impossibleError "unchecked variant"



------------------------------------------------------------------------
-- Variants
------------------------------------------------------------------------

type family Variants a :: [*]

type instance Variants (Rec   vs) = vs
type instance Variants (Maybe  a) = Variants a



------------------------------------------------------------------------
-- Records
------------------------------------------------------------------------

-- | A Record is any datatype implementing both Record as well as HasRecord type classes.
-- | Each record contains its layout definition Rec.

data Rec (lst :: [*]) where
    Data :: ! t        -> Rec (t ': ts)
    Rec  :: ! (Rec ts) -> Rec (t ': ts)

type VariantRec a = Rec (Variants a)

class Record r where
    mkRecord :: VariantRec r -> r

type  HasRecord' r = HasRecord r r
class HasRecord  r g where
    record :: Lens r g (VariantRec r) (VariantRec g)

-- utils

subRec    :: Rec (t ': ts) -> Rec ts
recData   :: Rec (t ': ts) -> t
unpackRec :: Rec (t ': ts) -> Either (Rec ts) t

subRec    (Rec  r) = r
recData   (Data t) = t
unpackRec rec      = ($ rec) $ case rec of
    Data _ -> Right . recData
    Rec  _ -> Left  . subRec

-- | The function `extend` adds new variant type on the beginning of the variant list.
-- | For simple records it looks like :: Rec vs -> Rec (v ': vs)
extend :: (HasRecord r t, Variants t ~ (t ': Variants r)) => r -> t
extend = record %~ Rec

-- | The function `narrow` removes the first variant from the variant list.
-- | For simple records it looks like :: Rec (v ': vs) -> Maybe (Rec vs)
narrow :: (HasRecord r g, Variants r ~ (t ': Variants g)) => r -> Maybe g
narrow = record $ \rec -> case unpackRec rec of
    Left  r -> Just r
    Right _ -> Nothing

-- instances

instance Record (Rec vs) where
    mkRecord = id

instance HasRecord (Rec vs) (Rec vs') where
    record   = lens id (flip const)


type RecordShow rec = Show (Rec (Variants rec))

instance                            Show (Rec '[]      ) where show _ = "EmptyRec"
instance (Show t, Show (Rec ts)) => Show (Rec (t ': ts)) where
    showsPrec d =  \case
        Data t -> showParen (d > app_prec) $ showString "Rec " . showsPrec (app_prec + 1) t
        Rec  r -> showsPrec d r
        where app_prec = 10


instance                                Repr s (Rec '[]      ) where repr _ = "EmptyRec"
instance (Repr s t, Repr s (Rec ts)) => Repr s (Rec (t ': ts)) where
    repr = \case
        Data t -> repr t
        Rec  r -> repr r

-- Repr

type VariantReprs s a = Reprs s (Variants a)

------------------------------------------------------------------------
-- Record bulk processing
------------------------------------------------------------------------

type WithRecVariantsM  ctx vs m vs' = WithVariantsM    ctx vs m (Rec vs')
type WithRecVariantsM' ctx a  m b   = WithRecVariantsM ctx (Variants a) m (Variants b)


class WithVariantsM ctx vs m b where
    withVariantsM :: Proxy ctx -> (forall v. ctx v m b => v -> m b) -> Rec vs -> m b

type WithRecVariants  ctx vs vs' = WithVariants    ctx vs           (Rec vs')
type WithRecVariants' ctx a  b   = WithRecVariants ctx (Variants a) (Variants b)

class WithVariants ctx vs b where
    withVariants :: Proxy ctx -> (forall v. ctx v b => v -> b) -> Rec vs -> b

type OverRecVariants' ctx a b = OverVariants ctx (Variants a) (Variants b)
class OverVariants ctx vs vs' where
    overVariants :: Proxy ctx -> (forall v v'. ctx v v' => v -> v') -> Rec vs -> Rec vs'

class Functor m => OverVariantsM ctx vs m vs' where
    overVariantsM :: Proxy ctx -> (forall v v'. ctx v m v' => v -> m v') -> Rec vs -> m (Rec vs')

-- instances

instance                                          WithVariantsM ctx '[]       m a where withVariantsM = error emptyRecordError
instance (ctx t m a, WithVariantsM ctx ts m a) => WithVariantsM ctx (t ': ts) m a where
    withVariantsM p f = \case
        Data t -> f t
        Rec  r -> withVariantsM p f r

instance                                     WithVariants ctx '[]       a where withVariants = error emptyRecordError
instance (ctx t a, WithVariants ctx ts a) => WithVariants ctx (t ': ts) a where
    withVariants p f = \case
        Data t -> f t
        Rec  r -> withVariants p f r


instance                                      OverVariants ctx '[]       a         where overVariants = error emptyRecordError
instance (ctx t l, OverVariants ctx ts ls) => OverVariants ctx (t ': ts) (l ': ls) where
    overVariants p f = \case
        Data t -> Data $ f t
        Rec  r -> Rec  $ overVariants p f r


instance  Functor m                                        => OverVariantsM ctx '[]       m a         where overVariantsM = error emptyRecordError
instance (Functor m, ctx t m l, OverVariantsM ctx ts m ls) => OverVariantsM ctx (t ': ts) m (l ': ls) where
    overVariantsM p f = \case
        Data t -> Data <$> f t
        Rec  r -> Rec  <$> overVariantsM p f r



------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------

-- RecCons      is the most primitive Rec constructor
-- CheckedCons  returns Found or NotFound records
-- SpecificCons performs construction only if it is possible, checked at compile time
-- MaybeCons    returns (Jut record) if possible or Nothing otherwise
-- Variant      the automatic constructor - chooses between Maybe- and Specific- Variant

-- types

data Found    a = Found    { fromVal :: a } deriving (Show, Functor)
data NotFound a = NotFound                  deriving (Show)

instance Functor NotFound where fmap _ _ = NotFound

instance ToMaybe Found    where toMaybe   = Just . fromVal
instance ToMaybe NotFound where toMaybe _ = Nothing


-- RecCons

class RecCons v vs where
    recCons :: v -> Rec vs

instance                 RecCons v (v ': vs) where recCons = Data
instance RecCons v vs => RecCons v (t ': vs) where recCons = Rec . recCons


-- CheckedCons

type CheckedCons v m vs = SafeCons (In v vs) v m vs

class ToMaybe m => SafeCons (ok :: Bool) v m vs | ok -> m where
    safeCons :: Proxy ok -> v -> m (Rec vs)

checkedCons :: forall ok v m vs. (ok ~ In v vs, SafeCons ok v m vs) => v -> m (Rec vs)
checkedCons = safeCons (Proxy :: Proxy ok)

instance RecCons v vs => SafeCons 'True  v    Found vs where safeCons _ = Found . recCons
instance                 SafeCons 'False v NotFound vs where safeCons _ = const NotFound


-- SpecificCons

class                                                          SpecificCons v rec where specificCons :: v -> rec
instance (SpecificCons' ok v rec, ok ~ In v (Variants rec)) => SpecificCons v rec where specificCons = specificCons' (Proxy :: Proxy ok)

class SpecificCons' (ok :: Bool) v rec where
    specificCons' :: Proxy ok -> v -> rec

instance (Record rec, CheckedCons v Found (Variants rec)) => SpecificCons' 'True v rec where
    specificCons' _ = mkRecord . fromVal . checkedCons

class    VariantNotBelongsTo v rec
instance VariantNotBelongsTo v rec => SpecificCons' 'False v rec where
    specificCons' = error uncheckedVariantError


-- MaybeCons

class Record rec => MaybeCons v rec where
    maybeCons :: v -> Maybe rec

instance (Record rec, CheckedCons v m (Variants rec)) => MaybeCons v rec where
    maybeCons = fmap mkRecord . toMaybe . checkedCons


-- Variant

class                                               Variant v        rec  where cons :: v -> rec
instance {-# OVERLAPPABLE #-} SpecificCons v rec => Variant v        rec  where cons = specificCons
instance {-# OVERLAPPABLE #-} MaybeCons    v rec => Variant v (Maybe rec) where cons = maybeCons



------------------------------------------------------------------------
-- Casting
------------------------------------------------------------------------

type Castable r r' = (WithVariants Variant (Variants r) r', HasRecord' r)

cast :: Castable r r' => r -> r'
cast = withVariants (Proxy :: Proxy Variant) cons . view record



------------------------------------------------------------------------
-- Variant Utilities
------------------------------------------------------------------------

-- === VariantShow ===

type VariantShow rec = WithVariants VariantsShow rec String

class (out ~ String) => VariantsShow v out where
    variantsShow :: v -> out

instance Show v => VariantsShow v String where
    variantsShow = show

variantShow :: VariantShow rec => Rec rec -> String
variantShow   = withVariants (Proxy :: Proxy VariantsShow) variantsShow


-- === Variant map ===

data ANY = ANY deriving (Show, Typeable)

class (m ~ Maybe, b~out) => MaybeMap' a b v m out where
    tryMap' :: (a -> b) -> v -> m out

instance (BaseType (Proxy a) pba, BaseType (Proxy b) pbb, ok ~ (pba :== pbb), MaybeMapHelper ok a b out)
      => MaybeMap' a   out b Maybe out where tryMap'     = tryMapHelper (Proxy :: Proxy ok)
instance MaybeMap' ANY out b Maybe out where tryMap' f _ = Just $ f ANY

class             MaybeMapHelper (ok :: Bool) a v b where tryMapHelper :: Proxy ok -> (a -> b) -> v -> Maybe b
instance          MaybeMapHelper False        a v b where tryMapHelper _ _ _ = Nothing
instance a ~ v => MaybeMapHelper True         a v b where tryMapHelper _ f v = Just $ f v

type VariantMap a rec b = WithVariantsM (MaybeMap' a b) rec Maybe b

tryMap :: VariantMap a rec b => (a -> b) -> Rec rec -> Maybe b
tryMap (f :: (a -> b)) = withVariantsM (Proxy :: Proxy (MaybeMap' a b)) (tryMap' f)



------------------------------------------------------------------------
-- Functors, Foldables and Traversables
------------------------------------------------------------------------

-- === Functors ===

class                                      VariantFunctor a b  v     out   | b v -> out, out a -> v where variantFunctor :: (a -> b) -> v -> out
instance (out ~ v b, a ~ a', Functor v) => VariantFunctor a b (v a') (v b)                          where variantFunctor f a = fmap f a
instance {-# OVERLAPPABLE #-} out ~ v   => VariantFunctor a b  v     out                            where variantFunctor f a = a

recMap :: OverVariants (VariantFunctor a b) vs vs' => (a -> b) -> Rec vs -> Rec vs'
recMap (f :: (a -> b)) = overVariants (Proxy :: Proxy (VariantFunctor a b)) (variantFunctor f)

recordMap :: (OverVariants (VariantFunctor a b) (Variants (r a)) (Variants (r b)), HasRecord (r a) (r b)) => (a -> b) -> r a -> r b
recordMap f = record %~ recMap f


-- === Foldables ===

class    (out ~ b)            => VariantFoldable a b  v     out | b -> out, out -> b where variantFoldr :: (a -> b -> b) -> b -> v -> out
instance (a ~ a', Foldable v) => VariantFoldable a b (v a') b                        where variantFoldr f b v = foldr f b v
instance {-# OVERLAPPABLE #-}    VariantFoldable a b  v     b                        where variantFoldr f b v = b

recFoldr :: WithVariants (VariantFoldable a b) vs b => (a -> b -> b) -> b -> Rec vs -> b
recFoldr (f :: (a -> b -> b)) b = withVariants (Proxy :: Proxy (VariantFoldable a b)) (variantFoldr f b)

recordFoldr :: (WithVariants (VariantFoldable a b) (Variants (r a)) b, HasRecord' (r a)) => (a -> b -> b) -> b -> r a -> b
recordFoldr f b = recFoldr f b . view record


-- === Traversables ===

class                                   Applicative f  => VariantTraversable a b  v     f out  | b v -> out, out a -> v where variantTraverse :: (a -> f b) -> v -> f out
instance (a ~ a', Traversable v,        Applicative f) => VariantTraversable a b (v a') f (v b)                         where variantTraverse f v = traverse f v
instance {-# OVERLAPPABLE #-} (out ~ v, Applicative f) => VariantTraversable a b  v     f out                           where variantTraverse f v = pure v

recTraverse :: OverVariantsM (VariantTraversable a b) vs f vs' => (a -> f b) -> Rec vs -> f (Rec vs')
recTraverse (f :: (a -> f b)) = overVariantsM (Proxy :: Proxy (VariantTraversable a b)) (variantTraverse f)

recordTraverse :: (Functor f, OverVariantsM (VariantTraversable a b) (Variants (r a)) f (Variants (r b)), HasRecord (r a) (r b))
               => (a -> f b) -> r a -> f (r b)
recordTraverse = record . recTraverse



------------------------------------------------------------------------
-- Pattern Matching
------------------------------------------------------------------------

-- === Match ===

type Case a matches out = (HasRecord' a, IsMatchSet matches (Variants a) out)

newtype Match rec out = Match { runMatch :: Rec rec -> Maybe out } deriving (Typeable)

instance (Typeable rec, Typeable out) => Show (Match rec out) where show _ = show (typeOf (undefined :: Match rec out))

type MatchMonad v rec out = VariantMap v rec out

--- === MatchSet ===

type MatchSet rec out = State [Match rec out] ()

class IsMatchSet matches a out | matches -> a out where
    toMatchSet :: matches -> MatchSet a out


instance Show (MatchSet rec out) where show _ = "MatchSet"

instance IsMatchSet (MatchSet rec out) rec out where toMatchSet     = id
instance IsMatchSet [MatchSet rec out] rec out where toMatchSet lst = sequence lst *> pure ()
instance IsMatchSet [Match    rec out] rec out where toMatchSet     = put

-- === Utils ===

match :: MatchMonad v rec out => (v -> out) -> MatchSet rec out
match m = withState (<> [Match $ tryMap m])

runCase :: Rec rec -> MatchSet rec out -> Maybe out
runCase val s = case execState s mempty of
    (m:ms) -> runMatch m val `orElse` runCase val (toMatchSet ms)
    []     -> Nothing

secureCase :: IsMatchSet matches rec out
           => Rec rec -> matches -> Maybe out
secureCase rec matches = runCase rec $ toMatchSet matches

unsecureCase :: IsMatchSet matches rec out
              => Rec rec -> matches -> out
unsecureCase rec matches = case secureCase rec matches of
    Just  a -> a
    Nothing -> error "*** Exception: Non-exhaustive patterns in variant case"

case' :: Case a matches out => a -> matches -> out
case' = unsecureCase . view record



--------------------------------------------------------------------------
---- Tests
--------------------------------------------------------------------------

data A = A Int deriving (Show, Eq, Typeable)
data B = B Int deriving (Show, Eq, Typeable)
data C = C Int deriving (Show, Eq, Typeable)
data D a = D a deriving (Show, Eq, Typeable, Functor)

type Foox = Rec '[A]


type Foo1 = Rec '[A]
type Foo2 = Rec '[A,B]
type Foo3 = Rec '[A,B,C]
type FooD a = Rec '[A,B,C,D a]

type Foo2' = Rec '[B,A]

data X   = X { _xrec :: Foo1 } deriving (Show)
data Y a = Y { _yrec :: a    } deriving (Show)

data XD a = XD { _drec :: FooD a } deriving (Show)

makeLenses ''X
makeLenses ''Y
makeLenses ''XD
--recordMap :: (OverVariants (VariantFunctor a b) (Variants (r a)) (Variants (r b)), HasRecord (r a) (r b)) => (a -> b) -> r a -> r b

type instance Variants X      = '[A]
type instance Variants (Y a)  = Variants a
type instance Variants (XD a) = Variants (FooD a)

instance             Record X      where mkRecord = X
instance Record a => Record (Y a)  where mkRecord = Y . mkRecord
instance             Record (XD a) where mkRecord = XD

instance                  HasRecord X     X       where record = xrec
instance HasRecord a b => HasRecord (Y a) (Y b)   where record = yrec . record
instance                  HasRecord (XD a) (XD b) where record = drec



ofType :: a -> a -> a
ofType = const

test = do
    let x1 = cons (A 1) :: Foo1
        x2 = cons (B 1) :: Foo2
        x3 = cons (C 1) :: Foo3
        x4 = cons (A 1) :: Maybe Foo3
        x5 = cons (C 1) :: Maybe Foo3
        x6 = cons (C 1) :: Maybe Foo1

        t1 = cons (A 1) :: X

        d1 = cons (D (5 :: Int)) :: FooD Int
        --d2 = recMap show d1 :: FooD String
        d3 = cons (D (5 :: Int)) :: XD Int
        fdc ff = recordMap show d3 :: XD String

        --d2 = recMap show d1
    --    --x7 = cons (B 1) :: Foo1


    print $ x1
    print $ x2
    print $ x3
    print $ x4
    print $ x5
    print $ x6
    print "---"

    print d1
    print d3
    print $ recordMap show d3

    print "---"

    print $ t1
    print "---"

    let y1 = cast x1 :: Foo1
        y2 = cast x2 :: Foo3
        y3 = cast x2 :: Maybe Foo1
        --y4 = cast x2 :: Foo1

    print $ y1
    print $ y2
    print $ y3
    print "---"

    let x1'  = specificCons (A 1) :: Foo1
        x1'' = checkedCons  (A 1) :: Found Foo1
        x2'' = checkedCons  (B 1) :: NotFound Foo1


    let tst = cons (B 1) :: Foo2
        --tst' = tst :: Foo2'

    print $ case' tst $ do
        match $ \(A a) -> "a"
        match $ \(B a) -> "b"
        match $ \ANY   -> "x"

    print $ case' t1 $ do
        match $ \(A a) -> "a"
        match $ \(B a) -> "b"
        match $ \ANY   -> "x"

    putStrLn "---"
    putStrLn ""

    let p1 = cons (D (5 :: Int)) :: XD Int
    print $ case' p1 $ do
        match $ \(D a) -> ("foo" :: String)
        match $ \ANY -> "x"

    putStrLn "==="
    putStrLn ""


--main = test