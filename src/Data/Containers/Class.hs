{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}

-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
--{-# LANGUAGE PolyKinds      #-}


module Data.Containers.Class where

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

import Data.TypeLevel.Bool

import Data.Containers.Poly
import GHC.Prim

import Control.Monad.Trans.Maybe


data Impossible    = Impossible  deriving (Show)
data ImpossibleM a = ImpossibleM deriving (Show, Functor)

impossible = error "Impossible happened."

-- #define defOp(nameqsm, nameinfo, params, fname, sig) class nameqsm params cont m q s where fname :: (AssumeQuery info q s, info ~ nameinfo params cont) => Query q s -> info -> sig
-- defOp(MeasurableQSM,MeasurableInfo,,sizeQSM,cont -> SelResultM info s m Int)

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

type SelResult  info s     = Result (SelTags info s)
type SelResultM info s m a = m (SelResult info s a)


type QueryResult  info q t     = Result (QueryTags (info (DataStoreOf (ContainerOf t))) (FilterMutable q))
type QueryResultM info q t m a = m (Result (QueryTags (info (DataStoreOf (ContainerOf t))) (FilterMutable q)) a)


class MeasurableQSM cont m q s where sizeQSM      :: (AssumeQuery info q s, info ~ MeasurableInfo cont) => Query q s -> info -> cont -> SelResultM info s m Int
class MinIndexedQSM cont m q s where minIndexQSM  :: (AssumeQuery info q s, info ~ MinIndexedInfo cont) => Query q s -> info -> cont -> SelResultM info s m (IndexOf' (DataStoreOf cont))
class MaxIndexedQSM cont m q s where maxIndexQSM  :: (AssumeQuery info q s, info ~ MaxIndexedInfo cont) => Query q s -> info -> cont -> SelResultM info s m (IndexOf' (DataStoreOf cont))

type MeasurableInfo = RawInfo MeasurableQSM
type MinIndexedInfo = RawInfo MinIndexedQSM
type MaxIndexedInfo = RawInfo MaxIndexedQSM


class                         CTXOO MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM :: Proxy q -> t -> QueryResultM MeasurableInfo q t m Int
instance {-# OVERLAPPABLE #-} CTXOO MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM = barTy (runModsF' sizeQSM) (Proxy :: Proxy MeasurableQSM)
instance {-# OVERLAPPABLE #-} CTXOO MeasurableInfo q m Impossible  => MeasurableQM q m Impossible  where sizeQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO MeasurableInfo q ImpossibleM t => MeasurableQM q ImpossibleM t where sizeQM = impossible

class                         CTXOO MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM :: Proxy q -> t -> QueryResultM MinIndexedInfo q t m (IndexOf' (DataStoreOf t))
instance {-# OVERLAPPABLE #-} CTXOO MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM = barTy (runModsF' minIndexQSM) (Proxy :: Proxy MinIndexedQSM)
instance {-# OVERLAPPABLE #-} CTXOO MinIndexedInfo q m Impossible  => MinIndexedQM q m Impossible  where minIndexQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO MinIndexedInfo q ImpossibleM t => MinIndexedQM q ImpossibleM t where minIndexQM = impossible

class                         CTXOO MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM :: Proxy q -> t -> QueryResultM MaxIndexedInfo q t m (IndexOf' (DataStoreOf t))
instance {-# OVERLAPPABLE #-} CTXOO MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM = barTy (runModsF' maxIndexQSM) (Proxy :: Proxy MaxIndexedQSM)
instance {-# OVERLAPPABLE #-} CTXOO MaxIndexedInfo q m Impossible  => MaxIndexedQM q m Impossible  where maxIndexQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO MaxIndexedInfo q ImpossibleM t => MaxIndexedQM q ImpossibleM t where maxIndexQM = impossible

--sizeM' :: (FuncTrans '[] (Proxy q -> t -> m (Result (QueryTags (MeasurableInfo (DataStoreOf (ContainerOf t))) (FilterMutable q)) Int)) w_, MeasurableQM q m t) => w_
sizeM' :: (FuncTrans '[] (Proxy q -> t -> QueryResultM MeasurableInfo q t m Int) f, MeasurableQM q m t) => f
sizeM' = transFunc $ optBuilder sizeQM
sizeM  = withTransFunc (fmap3 simplify) sizeM'
sizeMy = withTransFunc (\f q t -> runOperation q $ f q t) sizeM
size   = withTransFunc (fmap2 runIdentity) sizeMy
size'  = withTransFunc (fmap2 runIdentity) sizeM'

minIndexM' = transFunc $ optBuilder minIndexQM
minIndexM  = withTransFunc (fmap3 simplify) minIndexM'
minIndexMy = withTransFunc (\f q t -> runOperation q $ f q t) minIndexM
minIndex   = withTransFunc (fmap2 runIdentity) minIndexMy
minIndex'  = withTransFunc (fmap2 runIdentity) minIndexM'

maxIndexM' = transFunc $ optBuilder maxIndexQM
maxIndexM  = withTransFunc (fmap3 simplify) maxIndexM'
maxIndexMy = withTransFunc (\f q t -> runOperation q $ f q t) maxIndexM
maxIndex   = withTransFunc (fmap2 runIdentity) maxIndexMy
maxIndex'  = withTransFunc (fmap2 runIdentity) maxIndexM'


-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable


class SingletonQSM          el cont m q s where singletonQSM :: (AssumeQuery info q s, info ~ SingletonInfo  el cont) => Query q s -> info -> el          -> SelResultM info s m cont
class AllocableQSM             cont m q s where allocQSM     :: (AssumeQuery info q s, info ~ AllocableInfo     cont) => Query q s -> info -> Int         -> SelResultM info s m cont
class ExpandableQSM            cont m q s where expandQSM    :: (AssumeQuery info q s, info ~ ExpandableInfo    cont) => Query q s -> info ->        cont -> SelResultM info s m cont
class GrowableQSM              cont m q s where growQSM      :: (AssumeQuery info q s, info ~ GrowableInfo      cont) => Query q s -> info -> Int -> cont -> SelResultM info s m cont


class                         CTXOO (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM :: Proxy q -> el -> QueryResultM (SingletonInfo el) q t m t
instance {-# OVERLAPPABLE #-} CTXOO (SingletonInfo el) q m Impossible  => SingletonQM el q m Impossible  where singletonQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (SingletonInfo el) q ImpossibleM t => SingletonQM el q ImpossibleM t where singletonQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM q el = withFlipped (fmap (fillData (filterMutable q))) <$> ((fmap . fmap) fromContainer (tgdr el)) where
                                                                                                                     tgdr e  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection SingletonQSM (ContainerOf t) q) (FilterMutable (ModsOf SingletonQSM (ContainerOf t)))) )) <$> (runModsF' singletonQSM) (uniqueProxy q) e

class                         CTXOO AllocableInfo q m t           => AllocableQM q m t           where allocQM :: Proxy q -> Int -> QueryResultM AllocableInfo q t m t
instance {-# OVERLAPPABLE #-} CTXOO AllocableInfo q m Impossible  => AllocableQM q m Impossible  where allocQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO AllocableInfo q ImpossibleM t => AllocableQM q ImpossibleM t where allocQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO AllocableInfo q m t           => AllocableQM q m t           where allocQM q el = withFlipped (fmap (fillData (filterMutable q))) <$> ((fmap . fmap) fromContainer (tgdr el)) where
                                                                                                                     tgdr e  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection AllocableQSM (ContainerOf t) q) (FilterMutable (ModsOf AllocableQSM (ContainerOf t)))) )) <$> (runModsF' allocQSM) (uniqueProxy q) e

class                         CTXOO ExpandableInfo q m t           => ExpandableQM q m t           where expandQM :: Proxy q -> t -> QueryResultM ExpandableInfo q t m t
instance {-# OVERLAPPABLE #-} CTXOO ExpandableInfo q m t           => ExpandableQM q m t           where expandQM q t = barTx (runModsF' expandQSM) (Proxy :: Proxy ExpandableQSM) q t
instance {-# OVERLAPPABLE #-} CTXOO ExpandableInfo q ImpossibleM t => ExpandableQM q ImpossibleM t where expandQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO ExpandableInfo q m Impossible  => ExpandableQM q m Impossible  where expandQM = impossible

class                         CTXOO GrowableInfo q m t           => GrowableQM q m t           where growQM :: Proxy q -> Int -> t -> QueryResultM GrowableInfo q t m t
instance {-# OVERLAPPABLE #-} CTXOO GrowableInfo q m t           => GrowableQM q m t           where growQM q i t = barTx (runModsF' (\q' inf -> growQSM q' inf i)) (Proxy :: Proxy GrowableQSM) q t
instance {-# OVERLAPPABLE #-} CTXOO GrowableInfo q m Impossible  => GrowableQM q m Impossible  where growQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO GrowableInfo q ImpossibleM t => GrowableQM q ImpossibleM t where growQM = impossible


type instance IxedMode SingletonQSM  = Single
type instance IxedMode ExpandableQSM = Multi
type instance IxedMode AllocableQSM  = Multi
type instance IxedMode GrowableQSM   = Multi

type SingletonInfo el = ElInfo  el SingletonQSM
type AllocableInfo    = RawInfo    AllocableQSM
type ExpandableInfo   = RawInfo    ExpandableQSM
type GrowableInfo     = RawInfo    GrowableQSM

type ExpandableQ  q = ExpandableQM q Identity
type ExpandableM    = ExpandableQM '[]
type Expandable     = ExpandableM Identity


expandM' = transFunc $ optBuilder expandQM
expandM = withTransFunc (fmap3 simplify) expandM'
expandMy = withTransFunc (\f q t -> runOperation q $ f q t) expandM
expand  = withTransFunc (fmap2 runIdentity) expandMy
expand' = withTransFunc (fmap2 runIdentity) expandM'

singletonM' = transFunc $ optBuilder singletonQM
singletonM  = withTransFunc (fmap3 simplify) singletonM'
singletonMy = withTransFunc (\f q t -> runOperation q $ f q t) singletonM
singleton   = withTransFunc (fmap2 runIdentity) singletonMy
singleton'  = withTransFunc (fmap2 runIdentity) singletonM'

allocM' = transFunc $ optBuilder allocQM
allocM  = withTransFunc (fmap3 simplify) allocM'
allocMy = withTransFunc (\f q t -> runOperation q $ f q t) allocM
alloc   = withTransFunc (fmap2 runIdentity) allocMy
alloc'  = withTransFunc (fmap2 runIdentity) allocM

growM' = transFunc $ optBuilder growQM
growM  = withTransFunc (fmap4 simplify) growM'
growMy = withTransFunc (\f q i t -> runOperation q $ f q i t) growM
grow   = withTransFunc (fmap3 runIdentity) growMy
grow'  = withTransFunc (fmap3 runIdentity) growM'



-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable

class AppendableQSM           el cont m q s where appendQSM      :: (AssumeQuery info q s, info ~ AppendableInfo     el cont) => Query q s -> info ->        el -> cont -> SelResultM info s m cont
class AddableQSM              el cont m q s where addQSM         :: (AssumeQuery info q s, info ~ AddableInfo        el cont) => Query q s -> info ->        el -> cont -> SelResultM info s m cont
class InsertableQSM       idx el cont m q s where insertQSM      :: (AssumeQuery info q s, info ~ InsertableInfo idx el cont) => Query q s -> info -> idx -> el -> cont -> SelResultM info s m cont

type AppendableInfo     el = ElInfo          el AppendableQSM
type AddableInfo        el = ElInfo          el AddableQSM
type InsertableInfo idx el = IxedElInfo  idx el InsertableQSM

class                         CTXOO (AppendableInfo el) q m t           => AppendableQM el q m t           where appendQM :: Proxy q -> el -> t -> QueryResultM (AppendableInfo el) q t m t
instance {-# OVERLAPPABLE #-} CTXOO (AppendableInfo el) q m t           => AppendableQM el q m t           where appendQM q i t = barTx (runModsF' (\q' inf -> appendQSM q' inf i)) (Proxy :: Proxy AppendableQSM) q t
instance {-# OVERLAPPABLE #-} CTXOO (AppendableInfo el) q m Impossible  => AppendableQM el q m Impossible  where appendQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (AppendableInfo el) q ImpossibleM t => AppendableQM el q ImpossibleM t where appendQM = impossible

class                         CTXOO (AddableInfo el) q m t           => AddableQM el q m t           where addQM :: Proxy q -> el -> t -> QueryResultM (AddableInfo el) q t m t
instance {-# OVERLAPPABLE #-} CTXOO (AddableInfo el) q m t           => AddableQM el q m t           where addQM q i t = barTx (runModsF' (\q' inf -> addQSM q' inf i)) (Proxy :: Proxy AddableQSM) q t
instance {-# OVERLAPPABLE #-} CTXOO (AddableInfo el) q m Impossible  => AddableQM el q m Impossible  where addQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (AddableInfo el) q ImpossibleM t => AddableQM el q ImpossibleM t where addQM = impossible

class                         CTXOO (InsertableInfo idx el) q m t           => InsertableQM idx el q m t           where insertQM :: Proxy q -> idx -> el -> t -> QueryResultM (InsertableInfo idx el) q t m t
instance {-# OVERLAPPABLE #-} CTXOO (InsertableInfo idx el) q m t           => InsertableQM idx el q m t           where insertQM q i el t = barTx (runModsF' (\q' inf -> insertQSM q' inf i el)) (Proxy :: Proxy InsertableQSM) q t
instance {-# OVERLAPPABLE #-} CTXOO (InsertableInfo idx el) q m Impossible  => InsertableQM idx el q m Impossible  where insertQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (InsertableInfo idx el) q ImpossibleM t => InsertableQM idx el q ImpossibleM t where insertQM = impossible


type instance IxedMode AppendableQSM  = Single
type instance IxedMode AddableQSM     = Single
type instance IxedMode InsertableQSM  = Single


appendM' = transFunc $ optBuilder appendQM
appendM  = withTransFunc (fmap4 simplify) appendM'
appendMy = withTransFunc (\f q i t -> runOperation q $ f q i t) appendM
append   = withTransFunc (fmap3 runIdentity) appendMy
append'  = withTransFunc (fmap3 runIdentity) appendM'

addM' = transFunc $ optBuilder addQM
addM  = withTransFunc (fmap4 simplify) addM'
addMy = withTransFunc (\f q i t -> runOperation q $ f q i t) addM
add   = withTransFunc (fmap3 runIdentity) addMy
add'  = withTransFunc (fmap3 runIdentity) addM'

insertM' = transFunc $ optBuilder insertQM
insertM  = withTransFunc (fmap5 simplify) insertM'
insertMy = withTransFunc (\f q i t -> runOperation q $ f q i t) insertM
insert   = withTransFunc (fmap4 runIdentity) insertMy
insert'  = withTransFunc (fmap4 runIdentity) insertM'




---- === Indexing ===

-- Indexable
-- TracksElems
-- TracksIxes
-- TracksFreeIxes
-- TracksUsedIxes

class IndexableQSM                idx el cont m q s where indexQSM      :: (AssumeQuery info q s, info ~ IndexableInfo      idx el cont) => Query q s -> info -> idx -> cont -> SelResultM info s m el
class TracksFreeIxesQSM           idx    cont m q s where freeIxesQSM   :: (AssumeQuery info q s, info ~ TracksFreeIxesInfo idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]
class TracksUsedIxesQSM           idx    cont m q s where usedIxesQSM   :: (AssumeQuery info q s, info ~ TracksUsedIxesInfo idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]
class TracksIxesQSM               idx    cont m q s where ixesQSM       :: (AssumeQuery info q s, info ~ TracksIxesInfo     idx    cont) => Query q s -> info ->        cont -> SelResultM info s m [idx]

class                         CTXOO (TracksFreeIxesInfo idx) q m t           => TracksFreeIxesQM idx q m t           where freeIxesQM :: Proxy q -> t -> QueryResultM (TracksFreeIxesInfo idx) q t m [idx]
instance {-# OVERLAPPABLE #-} CTXOO (TracksFreeIxesInfo idx) q m t           => TracksFreeIxesQM idx q m t           where freeIxesQM = barTy (runModsF' freeIxesQSM) (Proxy :: Proxy TracksFreeIxesQSM)
instance {-# OVERLAPPABLE #-} CTXOO (TracksFreeIxesInfo idx) q m Impossible  => TracksFreeIxesQM idx q m Impossible  where freeIxesQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (TracksFreeIxesInfo idx) q ImpossibleM t => TracksFreeIxesQM idx q ImpossibleM t where freeIxesQM = impossible

class                         CTXOO (TracksUsedIxesInfo idx) q m t           => TracksUsedIxesQM idx q m t           where usedIxesQM :: Proxy q -> t -> QueryResultM (TracksUsedIxesInfo idx) q t m [idx]
instance {-# OVERLAPPABLE #-} CTXOO (TracksUsedIxesInfo idx) q m t           => TracksUsedIxesQM idx q m t           where usedIxesQM = barTy (runModsF' usedIxesQSM) (Proxy :: Proxy TracksUsedIxesQSM)
instance {-# OVERLAPPABLE #-} CTXOO (TracksUsedIxesInfo idx) q m Impossible  => TracksUsedIxesQM idx q m Impossible  where usedIxesQM = impossible
instance {-# OVERLAPPABLE #-} CTXOO (TracksUsedIxesInfo idx) q ImpossibleM t => TracksUsedIxesQM idx q ImpossibleM t where usedIxesQM = impossible

class                         CTXOO (TracksIxesInfo idx)     q m t           => TracksIxesQM     idx q m t           where ixesQM     :: Proxy q -> t -> QueryResultM (TracksIxesInfo idx) q t m [idx]
instance {-# OVERLAPPABLE #-} CTXOO (TracksIxesInfo idx)     q m t           => TracksIxesQM     idx q m t           where ixesQM     = barTy (runModsF' ixesQSM) (Proxy :: Proxy TracksIxesQSM)
instance {-# OVERLAPPABLE #-} CTXOO (TracksIxesInfo idx)     q m Impossible  => TracksIxesQM     idx q m Impossible  where ixesQM     = impossible
instance {-# OVERLAPPABLE #-} CTXOO (TracksIxesInfo idx)     q ImpossibleM t => TracksIxesQM     idx q ImpossibleM t where ixesQM     = impossible

class                         (SimpleRes el, CTXOO (IndexableInfo idx el) q m t          ) => IndexableQM idx el q m t           where indexQM :: Proxy q -> idx -> t -> QueryResultM (IndexableInfo idx el) q t m el
instance {-# OVERLAPPABLE #-} (SimpleRes el, CTXOO (IndexableInfo idx el) q m t          ) => IndexableQM idx el q m t           where indexQM q i t = barTy (runModsF' (\q' inf -> indexQSM q' inf i)) (Proxy :: Proxy IndexableQSM) q t
instance {-# OVERLAPPABLE #-} (SimpleRes el, CTXOO (IndexableInfo idx el) q m Impossible ) => IndexableQM idx el q m Impossible  where indexQM = impossible
instance {-# OVERLAPPABLE #-} (SimpleRes el, CTXOO (IndexableInfo idx el) q ImpossibleM t) => IndexableQM idx el q ImpossibleM t where indexQM = impossible



type IndexableInfo      idx el = IxedElInfo  idx el IndexableQSM
type TracksFreeIxesInfo idx    = IxedInfo    idx    TracksFreeIxesQSM
type TracksUsedIxesInfo idx    = IxedInfo    idx    TracksUsedIxesQSM
type TracksIxesInfo     idx    = IxedInfo    idx    TracksIxesQSM


type IndexableQ idx el q = IndexableQM idx el q Identity
type IndexableM idx el   = IndexableQM idx el '[]
type Indexable  idx el   = IndexableM  idx el Identity


freeIxesM' = transFunc $ optBuilder freeIxesQM
freeIxesM  = withTransFunc (fmap3 simplify) freeIxesM'
freeIxesMy = withTransFunc (\f q t -> runOperation q $ f q t) freeIxesM
freeIxes   = withTransFunc (fmap2 runIdentity) freeIxesMy
freeIxes'  = withTransFunc (fmap2 runIdentity) freeIxesM'


indexM' = transFunc $ optBuilder indexQM
indexM  = withTransFunc (fmap4 simplify) indexM'
indexMy = withTransFunc (\f q i t -> runOperation q $ f q i t) indexM
index   = withTransFunc (fmap3 runIdentity) indexMy
index'  = withTransFunc (fmap3 runIdentity) indexM'


lookup = try index





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



type family   AsTup a
type instance AsTup () = ()
type instance AsTup (t1,()) = t1
type instance AsTup (t1,(t2,())) = (t1,t2)
type instance AsTup (t1,(t2,(t3,()))) = (t1,t2,t3)
type instance AsTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
type instance AsTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)

type family   AsRTup a where
    AsRTup () = ()
    AsRTup (t1,t2) = (t1,(t2,()))
    AsRTup (t1,t2,t3) = (t1,(t2,(t3,())))
    AsRTup (t1,t2,t3,t4) = (t1,(t2,(t3,(t4,()))))
    AsRTup (t1,t2,t3,t4,t5) = (t1,(t2,(t3,(t4,(t5,())))))
    AsRTup (t1,t2,t3,t4,t5,t6) = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
    AsRTup (t1,t2,t3,t4,t5,t6,t7) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
    AsRTup (t1,t2,t3,t4,t5,t6,t7,t8) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
    AsRTup (t1,t2,t3,t4,t5,t6,t7,t8,t9) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
    AsRTup a = (a,())



class    a ~ AsRTup (AsTup a) => ToTup a  where toTup :: a -> AsTup a
instance ToTup () where toTup _ = ()
instance AsRTup t1 ~ (t1,()) => ToTup (t1,()) where toTup (t1,()) = t1
instance ToTup (t1,(t2,())) where toTup (t1,(t2,())) = (t1,t2)
instance ToTup (t1,(t2,(t3,()))) where toTup (t1,(t2,(t3,()))) = (t1,t2,t3)
instance ToTup (t1,(t2,(t3,(t4,())))) where toTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
instance ToTup (t1,(t2,(t3,(t4,(t5,()))))) where toTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)

type SimpleRes a = AsRTup a ~ (a, ())








runOperation :: MonadOperation (In Try q) m a m' a' => Proxy (q :: [*]) -> m a -> m' a'
runOperation (q :: Proxy q) = runOperationM (Proxy :: Proxy (In Try q))

class MonadOperation (try :: Bool) m a m' a' | try m -> m', try m' -> m, try a -> a', try a' -> a where
  runOperationM :: Proxy try -> m a -> m' a'

instance                    MonadOperation False m a m  a         where runOperationM _ = id
instance (m ~ MaybeT m') => MonadOperation True  m a m' (Maybe a) where runOperationM _ = runMaybeT



-- GHC BUG [TO BE REPORTED]: The Simplify type class fixes a Haskell's type system bug. After deleting the impossible case the inferred type is to simplified resulting in a constraint of
-- `ToTup a ~ AsTup (ToTup a)` which is cyclic and cannot be met.
class Simplify rt a s | rt a -> s, s -> rt a where simplify :: Result rt a -> s
instance {-# OVERLAPPABLE #-} ( AppendRTup res opts, ToTup rtup
                              , rtup ~ AsRTup tup
                              , rtup ~ AppendedRTup res opts
                              , rtup ~ AsRTup tup
                              , res ~ LastEl rtup
                              , tup ~ AsTup rtup
                              ) => Simplify opts       res tup               where simplify (Result d r) = toTup $ appendRTup r d
instance {-# OVERLAPPABLE #-}      Simplify Impossible res (ImpossibleM res) where simplify = impossible



--class    a ~ AsRTup (AsTup a) => ToTup a  where toTup :: a -> AsTup a


--class    a ~ LastEl (AppendedRTup a rt) => AppendRTup a rt                            where appendRTup :: a -> rt -> AppendedRTup a rt










withTransFunc = transFunc .: appFunc

appFunc :: (f -> g) -> OptBuilder opts f -> OptBuilder opts g
appFunc = fmap















setOptBuilder :: Proxy opts -> OptBuilder old a -> OptBuilder opts a
setOptBuilder _ (OptBuilder a) = OptBuilder a

type SetConstraint opts = FuncTrans opts f g => OptBuilder old f -> g


setConstraint :: Proxy opts -> SetConstraint opts
setConstraint = transFunc .: setOptBuilder



try       = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Try)
ixed      = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Ixed)
unchecked = transFunc $ optBuilder $ transFunc .: extendOptBuilder (Proxy :: Proxy Unchecked)

queried :: Proxy opts -> SetConstraint opts
queried = setConstraint



class    Cond (cond :: Bool) where ifT :: Proxy cond -> a -> a -> a
instance Cond True           where ifT _ = const
instance Cond False          where ifT _ = flip const



class UncheckedIf (cond :: Bool) opts opts' | cond opts -> opts', cond opts' -> opts where uncheckedIf :: Proxy cond -> OptBuilder opts f -> OptBuilder opts' f
instance UncheckedIf True  opts (Unchecked ': opts) where uncheckedIf _ (OptBuilder f) = OptBuilder f
instance UncheckedIf False opts opts                where uncheckedIf _ = id

uncheckedIf' :: (FuncTrans opts f y, UncheckedIf cond opts1 opts) => Proxy (cond :: Bool) -> OptBuilder opts1 f -> y
uncheckedIf' = transFunc .: uncheckedIf












class Result2 (i :: Bool) t f g | i t f -> g, i g -> t  where
    result2 :: a ~ ResultElem i f => Proxy i -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)

type family ResultElem i a where
    ResultElem True  (f a) = a
    ResultElem False a     = a

instance {-# OVERLAPPABLE #-} Result2 False t a     (t a)     where result2 _ lens f = lens %~ f
instance Functor f         => Result2 True  t (f a) (f (t a)) where result2 _ lens f = lens f

checkQuery :: Proxy q -> Proxy (In Ixed q)
checkQuery _ = Proxy



type Result2' q = Result2 (In Ixed q)

result2' :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g) => Proxy q -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)
result2' = result2 . checkQuery



wrappedResult :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g, Wrapped t) => Proxy q -> (a' -> f) -> (t a' -> g)
wrappedResult q = result2' q wrapped


runWrapped0 s f = wrappedResult (query s) $   f (polySpecX s)
runWrapped1 s f = wrappedResult (query s) .   f (polySpecX s)
runWrapped2 s f = wrappedResult (query s) .:  f (polySpecX s)
runWrapped3 s f = wrappedResult (query s) .:. f (polySpecX s)
runWrapped4 s f = wrappedResult (query s) .:: f (polySpecX s)

