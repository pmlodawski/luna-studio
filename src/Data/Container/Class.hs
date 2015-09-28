{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}

-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
--{-# LANGUAGE PolyKinds      #-}


module Data.Container.Class where

import           Prologue        hiding (Indexable, index, Bounded, Ixed, Simple, Indexed)
import           Data.Maybe             (fromJust)
import           Data.Typeable

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.TypeLevel.List (In)

import           Data.Container.TH
import qualified Data.Container.Mods as Mods

import Data.TypeLevel.Bool

import Data.Container.Poly
import GHC.Prim

import Control.Monad.Trans.Maybe


data Impossible    = Impossible  deriving (Show)
data ImpossibleM a = ImpossibleM deriving (Show, Functor)

impossible = error "Impossible happened."

-- #define defOp(nameqsm, nameinfo, params, fname, sig) class nameqsm params cont m q s where fname :: (AssumeQuery info q s, info ~ nameinfo params cont) => Query q s -> info -> sig
-- defOp(MeasurableQSM,MeasurableInfo,,sizeQSM,cont -> SelResultM info s m Int)

-- TODO[WD]: Finish TH
--sig = undefined
--mkQM "Measurable2" "size2" [| sig :: Int |]  [|barTy (runModsF' sizeQSM) (Proxy :: Proxy MeasurableQSM)|]


type SelResult  info s     = Result (SelTags info s)
type SelResultM info s m a = m (SelResult info s a)

type QueryResultData info q t     = QueryTags (info (DataStoreOf (ContainerOf t))) (Mods.FilterMutable q)
type QueryResult     info q t     = Result (QueryResultData info q t)
type QueryResultM    info q m t a = m (Result (QueryTags (info (DataStoreOf (ContainerOf t))) (Mods.FilterMutable q)) a)

type Func (q :: [*]) sig = FuncTrans '[] (Proxy q -> sig) f => f


-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class MeasurableQSM cont m q s where sizeQSM      :: (AssumeQuery info q s, info ~ MeasurableInfo cont) => Query q s -> info -> cont -> SelResultM info s m Int
class MinIndexedQSM cont m q s where minIndexQSM  :: (AssumeQuery info q s, info ~ MinIndexedInfo cont) => Query q s -> info -> cont -> SelResultM info s m (IndexOf' (DataStoreOf cont))
class MaxIndexedQSM cont m q s where maxIndexQSM  :: (AssumeQuery info q s, info ~ MaxIndexedInfo cont) => Query q s -> info -> cont -> SelResultM info s m (IndexOf' (DataStoreOf cont))


class                         OpCtx MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM :: Proxy q -> t -> QueryResultM MeasurableInfo q m t Int
instance {-# OVERLAPPABLE #-} OpCtx MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM = barTy (runModsF' sizeQSM) (Proxy :: Proxy MeasurableQSM)
instance {-# OVERLAPPABLE #-} OpCtx MeasurableInfo q m Impossible  => MeasurableQM q m Impossible  where sizeQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx MeasurableInfo q ImpossibleM t => MeasurableQM q ImpossibleM t where sizeQM = impossible

type MeasurableInfo   = RawInfo MeasurableQSM
type MeasurableQ    q = MeasurableQM q Identity
type MeasurableM      = MeasurableQM '[]
type Measurable       = MeasurableM  Identity

sizeM' :: MeasurableQM q m t => Func q (t -> QueryResultM MeasurableInfo q m t Int)
size'  :: MeasurableQ  q   t => Func q (t -> QueryResult  MeasurableInfo q   t Int)
sizeM  :: MeasurableQM q m t => (Simplify (QueryResultData MeasurableInfo q t) Int a, CheckOpM q m a m' a') => Func q (t -> m' a')
size   :: MeasurableQM q m t => (Simplify (QueryResultData MeasurableInfo q t) Int a, CheckOp  q m a    a') => Func q (t ->    a')

sizeM' = transFunc $ optBuilder sizeQM
size'  = withTransFunc (fmap2 runIdentity) sizeM'
sizeM  = withTransFunc (runOperation1 . fmap3 simplify) sizeM'
size   = withTransFunc (fmap2 runIdentity) sizeM

--

class                         OpCtx MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM :: Proxy q -> t -> QueryResultM MinIndexedInfo q m t (IndexOf' (DataStoreOf t))
instance {-# OVERLAPPABLE #-} OpCtx MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM = barTy (runModsF' minIndexQSM) (Proxy :: Proxy MinIndexedQSM)
instance {-# OVERLAPPABLE #-} OpCtx MinIndexedInfo q m Impossible  => MinIndexedQM q m Impossible  where minIndexQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx MinIndexedInfo q ImpossibleM t => MinIndexedQM q ImpossibleM t where minIndexQM = impossible

type MinIndexedInfo   = RawInfo MinIndexedQSM
type MinIndexedQ    q = MinIndexedQM q Identity
type MinIndexedM      = MinIndexedQM '[]
type MinIndexed       = MinIndexedM  Identity

minIndexM' :: MinIndexedQM q m t => Func q (t -> QueryResultM MinIndexedInfo q m t (IndexOf' (DataStoreOf t)))
minIndex'  :: MinIndexedQ  q   t => Func q (t -> QueryResult  MinIndexedInfo q   t (IndexOf' (DataStoreOf t)))
minIndexM  :: MinIndexedQM q m t => (Simplify (QueryResultData MinIndexedInfo q t) (IndexOf' (DataStoreOf t)) a, CheckOpM q m a m' a') => Func q (t -> m' a')
minIndex   :: MinIndexedQM q m t => (Simplify (QueryResultData MinIndexedInfo q t) (IndexOf' (DataStoreOf t)) a, CheckOp  q m a    a') => Func q (t ->    a')

minIndexM' = transFunc $ optBuilder minIndexQM
minIndex'  = withTransFunc (fmap2 runIdentity) minIndexM'
minIndexM  = withTransFunc (runOperation1 . fmap3 simplify) minIndexM'
minIndex   = withTransFunc (fmap2 runIdentity) minIndexM

--

class                         OpCtx MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM :: Proxy q -> t -> QueryResultM MaxIndexedInfo q m t (IndexOf' (DataStoreOf t))
instance {-# OVERLAPPABLE #-} OpCtx MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM = barTy (runModsF' maxIndexQSM) (Proxy :: Proxy MaxIndexedQSM)
instance {-# OVERLAPPABLE #-} OpCtx MaxIndexedInfo q m Impossible  => MaxIndexedQM q m Impossible  where maxIndexQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx MaxIndexedInfo q ImpossibleM t => MaxIndexedQM q ImpossibleM t where maxIndexQM = impossible

type MaxIndexedInfo   = RawInfo MaxIndexedQSM
type MaxIndexedQ    q = MaxIndexedQM q Identity
type MaxIndexedM      = MaxIndexedQM '[]
type MaxIndexed       = MaxIndexedM  Identity

maxIndexM' = transFunc $ optBuilder maxIndexQM
maxIndex'  = withTransFunc (fmap2 runIdentity) maxIndexM'
maxIndexM  = withTransFunc (runOperation1 . fmap3 simplify) maxIndexM'
maxIndex   = withTransFunc (fmap2 runIdentity) maxIndexM



-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable


class SingletonQSM          el cont m q s where singletonQSM :: (AssumeQuery info q s, info ~ SingletonInfo  el cont) => Query q s -> info -> el          -> SelResultM info s m cont
class AllocableQSM             cont m q s where allocQSM     :: (AssumeQuery info q s, info ~ AllocableInfo     cont) => Query q s -> info -> Int         -> SelResultM info s m cont
class ExpandableQSM            cont m q s where expandQSM    :: (AssumeQuery info q s, info ~ ExpandableInfo    cont) => Query q s -> info ->        cont -> SelResultM info s m cont
class GrowableQSM              cont m q s where growQSM      :: (AssumeQuery info q s, info ~ GrowableInfo      cont) => Query q s -> info -> Int -> cont -> SelResultM info s m cont


class                         OpCtx (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM :: Proxy q -> el -> QueryResultM (SingletonInfo el) q m t t
instance {-# OVERLAPPABLE #-} OpCtx (SingletonInfo el) q m Impossible  => SingletonQM el q m Impossible  where singletonQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (SingletonInfo el) q ImpossibleM t => SingletonQM el q ImpossibleM t where singletonQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM q el = withFlipped (fmap (fillData (Mods.filterMutable q))) <$> ((fmap . fmap) fromContainer (tgdr el)) where
                                                                                                                     tgdr e  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection SingletonQSM (ContainerOf t) q) (Mods.FilterMutable (ModsOf SingletonQSM (ContainerOf t)))) )) <$> (runModsF' singletonQSM) (uniqueProxy q) e
type instance IxedMode SingletonQSM  = Single
type SingletonInfo el   = ElInfo       el SingletonQSM
type SingletonQ    el q = SingletonQM el q Identity
type SingletonM    el   = SingletonQM el '[]
type Singleton     el   = SingletonM  el Identity

singletonM' = transFunc $ optBuilder singletonQM
singleton'  = withTransFunc (fmap2 runIdentity) singletonM'
singletonM  = withTransFunc (runOperation1 . fmap3 simplify) singletonM'
singleton   = withTransFunc (fmap2 runIdentity) singletonM

--

class                         OpCtx AllocableInfo q m t           => AllocableQM q m t           where allocQM :: Proxy q -> Int -> QueryResultM AllocableInfo q m t t
instance {-# OVERLAPPABLE #-} OpCtx AllocableInfo q m Impossible  => AllocableQM q m Impossible  where allocQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx AllocableInfo q ImpossibleM t => AllocableQM q ImpossibleM t where allocQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx AllocableInfo q m t           => AllocableQM q m t           where allocQM q el = withFlipped (fmap (fillData (Mods.filterMutable q))) <$> ((fmap . fmap) fromContainer (tgdr el)) where
                                                                                                                     tgdr e  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection AllocableQSM (ContainerOf t) q) (Mods.FilterMutable (ModsOf AllocableQSM (ContainerOf t)))) )) <$> (runModsF' allocQSM) (uniqueProxy q) e
type instance IxedMode AllocableQSM  = Multi
type AllocableInfo   = RawInfo     AllocableQSM
type AllocableQ    q = AllocableQM q Identity
type AllocableM      = AllocableQM '[]
type Allocable       = AllocableM  Identity

allocM' = transFunc $ optBuilder allocQM
alloc'  = withTransFunc (fmap2 runIdentity) allocM
allocM  = withTransFunc (runOperation1 . fmap3 simplify) allocM'
alloc   = withTransFunc (fmap2 runIdentity) allocM

--

class                         OpCtx ExpandableInfo q m t           => ExpandableQM q m t           where expandQM :: Proxy q -> t -> QueryResultM ExpandableInfo q m t t
instance {-# OVERLAPPABLE #-} OpCtx ExpandableInfo q m t           => ExpandableQM q m t           where expandQM q t = barTx (runModsF' expandQSM) (Proxy :: Proxy ExpandableQSM) q t
instance {-# OVERLAPPABLE #-} OpCtx ExpandableInfo q ImpossibleM t => ExpandableQM q ImpossibleM t where expandQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx ExpandableInfo q m Impossible  => ExpandableQM q m Impossible  where expandQM = impossible

type instance IxedMode ExpandableQSM = Multi
type ExpandableInfo   = RawInfo     ExpandableQSM
type ExpandableQ    q = ExpandableQM q Identity
type ExpandableM      = ExpandableQM '[]
type Expandable       = ExpandableM  Identity

expandM' = transFunc $ optBuilder expandQM
expand' = withTransFunc (fmap2 runIdentity) expandM'
expandM = withTransFunc (runOperation1 . fmap3 simplify) expandM'
expand  = withTransFunc (fmap2 runIdentity) expandM

--

class                         OpCtx GrowableInfo q m t           => GrowableQM q m t           where growQM :: Proxy q -> Int -> t -> QueryResultM GrowableInfo q m t t
instance {-# OVERLAPPABLE #-} OpCtx GrowableInfo q m t           => GrowableQM q m t           where growQM q i t = barTx (runModsF' (\q' inf -> growQSM q' inf i)) (Proxy :: Proxy GrowableQSM) q t
instance {-# OVERLAPPABLE #-} OpCtx GrowableInfo q m Impossible  => GrowableQM q m Impossible  where growQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx GrowableInfo q ImpossibleM t => GrowableQM q ImpossibleM t where growQM = impossible

type instance IxedMode GrowableQSM   = Multi
type GrowableInfo   = RawInfo     GrowableQSM
type GrowableQ    q = GrowableQM q Identity
type GrowableM      = GrowableQM '[]
type Growable       = GrowableM  Identity

growM' = transFunc $ optBuilder growQM
grow'  = withTransFunc (fmap3 runIdentity) growM'
growM  = withTransFunc (runOperation2 . fmap4 simplify) growM'
grow   = withTransFunc (fmap3 runIdentity) growM



-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable

class AppendableQSM           el cont m q s where appendQSM      :: (AssumeQuery info q s, info ~ AppendableInfo     el cont) => Query q s -> info ->        el -> cont -> SelResultM info s m cont
class AddableQSM              el cont m q s where addQSM         :: (AssumeQuery info q s, info ~ AddableInfo        el cont) => Query q s -> info ->        el -> cont -> SelResultM info s m cont
class InsertableQSM       idx el cont m q s where insertQSM      :: (AssumeQuery info q s, info ~ InsertableInfo idx el cont) => Query q s -> info -> idx -> el -> cont -> SelResultM info s m cont

--

class                         OpCtx (AppendableInfo el) q m t           => AppendableQM el q m t           where appendQM :: Proxy q -> el -> t -> QueryResultM (AppendableInfo el) q m t t
instance {-# OVERLAPPABLE #-} OpCtx (AppendableInfo el) q m t           => AppendableQM el q m t           where appendQM q i t = barTx (runModsF' (\q' inf -> appendQSM q' inf i)) (Proxy :: Proxy AppendableQSM) q t
instance {-# OVERLAPPABLE #-} OpCtx (AppendableInfo el) q m Impossible  => AppendableQM el q m Impossible  where appendQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (AppendableInfo el) q ImpossibleM t => AppendableQM el q ImpossibleM t where appendQM = impossible

type instance IxedMode AppendableQSM  = Single
type AppendableInfo el   = ElInfo       el AppendableQSM
type AppendableQ    el q = AppendableQM el q Identity
type AppendableM    el   = AppendableQM el '[]
type Appendable     el   = AppendableM  el Identity

appendM' = transFunc $ optBuilder appendQM
append'  = withTransFunc (fmap3 runIdentity) appendM'
appendM  = withTransFunc (runOperation2 . fmap4 simplify) appendM'
append   = withTransFunc (fmap3 runIdentity) appendM

--

class                         OpCtx (AddableInfo el) q m t           => AddableQM el q m t           where addQM :: Proxy q -> el -> t -> QueryResultM (AddableInfo el) q m t t
instance {-# OVERLAPPABLE #-} OpCtx (AddableInfo el) q m t           => AddableQM el q m t           where addQM q i t = barTx (runModsF' (\q' inf -> addQSM q' inf i)) (Proxy :: Proxy AddableQSM) q t
instance {-# OVERLAPPABLE #-} OpCtx (AddableInfo el) q m Impossible  => AddableQM el q m Impossible  where addQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (AddableInfo el) q ImpossibleM t => AddableQM el q ImpossibleM t where addQM = impossible

type instance IxedMode AddableQSM     = Single
type AddableInfo el   = ElInfo    el AddableQSM
type AddableQ    el q = AddableQM el q Identity
type AddableM    el   = AddableQM el '[]
type Addable     el   = AddableM  el Identity

addM' = transFunc $ optBuilder addQM
add'  = withTransFunc (fmap3 runIdentity) addM'
addM  = withTransFunc (runOperation2 . fmap4 simplify) addM'
add   = withTransFunc (fmap3 runIdentity) addM

--

class                         OpCtx (InsertableInfo idx el) q m t           => InsertableQM idx el q m t           where insertQM :: Proxy q -> idx -> el -> t -> QueryResultM (InsertableInfo idx el) q m t t
instance {-# OVERLAPPABLE #-} OpCtx (InsertableInfo idx el) q m t           => InsertableQM idx el q m t           where insertQM q i el t = barTx (runModsF' (\q' inf -> insertQSM q' inf i el)) (Proxy :: Proxy InsertableQSM) q t
instance {-# OVERLAPPABLE #-} OpCtx (InsertableInfo idx el) q m Impossible  => InsertableQM idx el q m Impossible  where insertQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (InsertableInfo idx el) q ImpossibleM t => InsertableQM idx el q ImpossibleM t where insertQM = impossible

type instance IxedMode InsertableQSM  = Single
type InsertableInfo idx el   = IxedElInfo   idx el InsertableQSM
type InsertableQ    idx el q = InsertableQM idx el q Identity
type InsertableM    idx el   = InsertableQM idx el '[]
type Insertable     idx el   = InsertableM  idx el Identity

insertM' = transFunc $ optBuilder insertQM
insert'  = withTransFunc (fmap4 runIdentity) insertM'
insertM  = withTransFunc (runOperation2 . fmap5 simplify) insertM'
insert   = withTransFunc (fmap4 runIdentity) insertM



---- === Indexing ===

-- Indexable
-- TracksElems
-- TracksIxes
-- TracksFreeIxes
-- TracksUsedIxes


class IndexableQSM      idx el cont m q s where indexQSM    :: (AssumeQuery info q s, info ~ IndexableInfo      idx el cont) => Query q s -> info -> idx -> cont -> SelResultM info s m el
class TracksFreeIxesQSM idx    cont m q s where freeIxesQSM :: (AssumeQuery info q s, info ~ TracksFreeIxesInfo idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]
class TracksUsedIxesQSM idx    cont m q s where usedIxesQSM :: (AssumeQuery info q s, info ~ TracksUsedIxesInfo idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]
class TracksIxesQSM     idx    cont m q s where ixesQSM     :: (AssumeQuery info q s, info ~ TracksIxesInfo     idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]

class                         OpCtx (TracksFreeIxesInfo idx) q m t           => TracksFreeIxesQM idx q m t           where freeIxesQM :: Proxy q -> t -> QueryResultM (TracksFreeIxesInfo idx) q m t [idx]
instance {-# OVERLAPPABLE #-} OpCtx (TracksFreeIxesInfo idx) q m t           => TracksFreeIxesQM idx q m t           where freeIxesQM = barTy (runModsF' freeIxesQSM) (Proxy :: Proxy TracksFreeIxesQSM)
instance {-# OVERLAPPABLE #-} OpCtx (TracksFreeIxesInfo idx) q m Impossible  => TracksFreeIxesQM idx q m Impossible  where freeIxesQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (TracksFreeIxesInfo idx) q ImpossibleM t => TracksFreeIxesQM idx q ImpossibleM t where freeIxesQM = impossible

type IndexableInfo idx el   = IxedElInfo  idx el IndexableQSM
type IndexableQ    idx el q = IndexableQM idx el q Identity
type IndexableM    idx el   = IndexableQM idx el '[]
type Indexable     idx el   = IndexableM  idx el Identity

freeIxesM' = transFunc $ optBuilder freeIxesQM
freeIxes'  = withTransFunc (fmap2 runIdentity) freeIxesM'
freeIxesM  = withTransFunc (runOperation1 . fmap3 simplify) freeIxesM'
freeIxes   = withTransFunc (fmap2 runIdentity) freeIxesM

--

class                         OpCtx (TracksUsedIxesInfo idx) q m t           => TracksUsedIxesQM idx q m t           where usedIxesQM :: Proxy q -> t -> QueryResultM (TracksUsedIxesInfo idx) q m t [idx]
instance {-# OVERLAPPABLE #-} OpCtx (TracksUsedIxesInfo idx) q m t           => TracksUsedIxesQM idx q m t           where usedIxesQM = barTy (runModsF' usedIxesQSM) (Proxy :: Proxy TracksUsedIxesQSM)
instance {-# OVERLAPPABLE #-} OpCtx (TracksUsedIxesInfo idx) q m Impossible  => TracksUsedIxesQM idx q m Impossible  where usedIxesQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx (TracksUsedIxesInfo idx) q ImpossibleM t => TracksUsedIxesQM idx q ImpossibleM t where usedIxesQM = impossible

type TracksFreeIxesInfo idx   = IxedInfo         idx TracksFreeIxesQSM
type TracksFreeIxesQ    idx q = TracksFreeIxesQM idx q Identity
type TracksFreeIxesM    idx   = TracksFreeIxesQM idx '[]
type TracksFreeIxes     idx   = TracksFreeIxesM  idx Identity

usedIxesM' = transFunc $ optBuilder usedIxesQM
usedIxes'  = withTransFunc (fmap2 runIdentity) usedIxesM'
usedIxesM  = withTransFunc (runOperation1 . fmap3 simplify) usedIxesM'
usedIxes   = withTransFunc (fmap2 runIdentity) usedIxesM

--

class                         OpCtx (TracksIxesInfo idx)     q m t           => TracksIxesQM     idx q m t           where ixesQM     :: Proxy q -> t -> QueryResultM (TracksIxesInfo idx) q m t [idx]
instance {-# OVERLAPPABLE #-} OpCtx (TracksIxesInfo idx)     q m t           => TracksIxesQM     idx q m t           where ixesQM     = barTy (runModsF' ixesQSM) (Proxy :: Proxy TracksIxesQSM)
instance {-# OVERLAPPABLE #-} OpCtx (TracksIxesInfo idx)     q m Impossible  => TracksIxesQM     idx q m Impossible  where ixesQM     = impossible
instance {-# OVERLAPPABLE #-} OpCtx (TracksIxesInfo idx)     q ImpossibleM t => TracksIxesQM     idx q ImpossibleM t where ixesQM     = impossible

type TracksUsedIxesInfo idx   = IxedInfo         idx TracksUsedIxesQSM
type TracksUsedIxesQ    idx q = TracksUsedIxesQM idx q Identity
type TracksUsedIxesM    idx   = TracksUsedIxesQM idx '[]
type TracksUsedIxes     idx   = TracksUsedIxesM  idx Identity

ixesM' = transFunc $ optBuilder ixesQM
ixes'  = withTransFunc (fmap2 runIdentity) ixesM'
ixesM  = withTransFunc (runOperation1 . fmap3 simplify) ixesM'
ixes   = withTransFunc (fmap2 runIdentity) ixesM

--

class                         (SimpleRes el, OpCtx (IndexableInfo idx el) q m t          ) => IndexableQM idx el q m t           where indexQM :: Proxy q -> idx -> t -> QueryResultM (IndexableInfo idx el) q m t el
instance {-# OVERLAPPABLE #-} (SimpleRes el, OpCtx (IndexableInfo idx el) q m t          ) => IndexableQM idx el q m t           where indexQM q i t = barTy (runModsF' (\q' inf -> indexQSM q' inf i)) (Proxy :: Proxy IndexableQSM) q t
instance {-# OVERLAPPABLE #-} (SimpleRes el, OpCtx (IndexableInfo idx el) q m Impossible ) => IndexableQM idx el q m Impossible  where indexQM = impossible
instance {-# OVERLAPPABLE #-} (SimpleRes el, OpCtx (IndexableInfo idx el) q ImpossibleM t) => IndexableQM idx el q ImpossibleM t where indexQM = impossible

type TracksIxesInfo idx   = IxedInfo     idx TracksIxesQSM
type TracksIxesQ    idx q = TracksIxesQM idx q Identity
type TracksIxesM    idx   = TracksIxesQM idx '[]
type TracksIxes     idx   = TracksIxesM  idx Identity

indexM' = transFunc $ optBuilder indexQM
index'  = withTransFunc (fmap3 runIdentity) indexM'
indexM  = withTransFunc (runOperation2 . fmap4 simplify) indexM'
index   = withTransFunc (fmap3 runIdentity) indexM

lookup = try index



-- TODO[WD]: refactor needed
-- === Utils ===


type family MappedRTup a tup where
    MappedRTup a (t, ()) = (a, ())
    MappedRTup a (t, ts) = (t, MappedRTup a ts)

class                                                                                              RTupFunctor a b tup    where fmapRTup :: (a -> b) -> tup -> MappedRTup b tup
instance {-# OVERLAPPABLE #-} (a ~ t)                                                           => RTupFunctor a b (t,()) where fmapRTup f (t,()) = (f t,())
instance {-# OVERLAPPABLE #-} (RTupFunctor a b ts, MappedRTup b (t, ts) ~ (t, MappedRTup b ts)) => RTupFunctor a b (t,ts) where fmapRTup f (t,ts) = (t, fmapRTup f ts)


type family AppendedRTup a rt where
    AppendedRTup a ()     = (a,())
    AppendedRTup a (t,ts) = (t,AppendedRTup a ts)

type family LastEl a
type instance LastEl (t,ts) = If (ts :== ()) t (LastEl ts)

class    a ~ LastEl (AppendedRTup a rt) => AppendRTup a rt                            where appendRTup :: a -> rt -> AppendedRTup a rt
instance  AppendRTup a ()                                                             where appendRTup a _      = (a,())
instance (AppendRTup a ts, a ~ LastEl (AppendedRTup a (t,ts))) => AppendRTup a (t,ts) where appendRTup a (t,ts) = (t,appendRTup a ts)












type CheckOp  q m a    a' = CheckOpM q m a Identity a'
type CheckOpM q m a m' a' = OpM (In Mods.Try q) m a m' a'

runOperation :: CheckOpM q m a m' a' => Proxy (q :: [*]) -> m a -> m' a'
runOperation (q :: Proxy q) = runOperationM (Proxy :: Proxy (In Mods.Try q))

runOperation1 f q = runOperation q .  f q
runOperation2 f q = runOperation q .: f q

class OpM (try :: Bool) m a m' a' | try m -> m', try m' -> m, try a -> a', try a' -> a where
  runOperationM :: Proxy try -> m a -> m' a'

instance                    OpM False m a m  a         where runOperationM _ = id
instance (m ~ MaybeT m') => OpM True  m a m' (Maybe a) where runOperationM _ = runMaybeT



-- GHC BUG [TO BE REPORTED]: The Simplify type class fixes a Haskell's type system bug. After deleting the impossible case the inferred type is to simplified resulting in a constraint of
-- `ToTup a ~ AsTup (ToTup a)` which is cyclic and cannot be met.
class Simplify rt a s | rt a -> s, s -> rt a where simplify :: Result rt a -> s
instance {-# OVERLAPPABLE #-} ( AppendRTup res opts, ToTup rtup
                              , rtup ~ AsRTup       tup
                              , rtup ~ AppendedRTup res opts
                              , res  ~ LastEl       rtup
                              , tup  ~ AsTup        rtup
                              ) => Simplify opts       res tup               where simplify (Result d r) = toTup $ appendRTup r d
instance {-# OVERLAPPABLE #-}      Simplify Impossible res (ImpossibleM res) where simplify = impossible






withTransFunc = transFunc .: appFunc

appFunc :: (f -> g) -> OptBuilder opts f -> OptBuilder opts g
appFunc = fmap















setOptBuilder :: Proxy opts -> OptBuilder old a -> OptBuilder opts a
setOptBuilder _ (OptBuilder a) = OptBuilder a

type SetConstraint opts = FuncTrans opts f g => OptBuilder old f -> g


setConstraint :: Proxy opts -> SetConstraint opts
setConstraint = transFunc .: setOptBuilder



try       = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Mods.Try)
ixed      = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Mods.Ixed)
unchecked = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Mods.Unchecked)

queried :: Proxy opts -> SetConstraint opts
queried = setConstraint



class    Cond (cond :: Bool) where ifT :: Proxy cond -> a -> a -> a
instance Cond True           where ifT _ = const
instance Cond False          where ifT _ = flip const



class UncheckedIf (cond :: Bool) opts opts' | cond opts -> opts', cond opts' -> opts where uncheckedIf :: Proxy cond -> OptBuilder opts f -> OptBuilder opts' f
instance UncheckedIf True  opts (Mods.Unchecked ': opts) where uncheckedIf _ (OptBuilder f) = OptBuilder f
instance UncheckedIf False opts opts                     where uncheckedIf _ = id

uncheckedIf' :: (FuncTrans opts f y, UncheckedIf cond opts1 opts) => Proxy (cond :: Bool) -> OptBuilder opts1 f -> y
uncheckedIf' = transFunc .: uncheckedIf












class Result2 (i :: Bool) t f g | i t f -> g, i g -> t  where
    result2 :: a ~ ResultElem i f => Proxy i -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)

type family ResultElem i a where
    ResultElem True  (f a) = a
    ResultElem False a     = a

instance {-# OVERLAPPABLE #-} Result2 False t a     (t a)     where result2 _ lens f = lens %~ f
instance Functor f         => Result2 True  t (f a) (f (t a)) where result2 _ lens f = lens f

checkQuery :: Proxy q -> Proxy (In Mods.Ixed q)
checkQuery _ = Proxy



type Result2' q = Result2 (In Mods.Ixed q)

result2' :: (a ~ ResultElem i f, i ~ In Mods.Ixed q, Result2 i t f g) => Proxy q -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)
result2' = result2 . checkQuery



wrappedResult :: (a ~ ResultElem i f, i ~ In Mods.Ixed q, Result2 i t f g, Wrapped t) => Proxy q -> (a' -> f) -> (t a' -> g)
wrappedResult q = result2' q wrapped


runWrapped0 s f = wrappedResult (query s) $   f (polySpecX s)
runWrapped1 s f = wrappedResult (query s) .   f (polySpecX s)
runWrapped2 s f = wrappedResult (query s) .:  f (polySpecX s)
runWrapped3 s f = wrappedResult (query s) .:. f (polySpecX s)
runWrapped4 s f = wrappedResult (query s) .:: f (polySpecX s)

