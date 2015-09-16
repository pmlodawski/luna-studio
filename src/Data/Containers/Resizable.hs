{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Containers.Resizable where

import Prologue              hiding (Indexable, index, Bounded, Simple)
import Data.Containers.Class
import Data.Typeable
import qualified Data.Containers.Interface as I
import           Data.Containers.Poly {- x -}
import           Data.TypeLevel.List (In)


--type Result q s = Result2' q (Resizable s)



-- === Resizable wrapper

data Resizable style a = Resizable !style !a deriving (Show)

instance              Unwrap  (Resizable l) where  unwrap (Resizable _ a) = a
instance Default l => Wrap    (Resizable l) where    wrap = Resizable def
instance              Wrapped (Resizable l) where wrapped = lens unwrap $ \(Resizable l _) a -> Resizable l a


instance (Default l, Monoid a) => Monoid (Resizable l a) where
    mempty                                   = Resizable def mempty
    mappend (Resizable l a) (Resizable _ a') = Resizable l (a <> a')

instance (Default l, Monoid a) => Default (Resizable l a) where
    def = Resizable def mempty

instance (IsList a, Default l) => IsList (Resizable l a) where
    type Item (Resizable l a) = Item a
    fromList = Resizable def . fromList

instance HasDataStore a => HasDataStore (Resizable l a) where dataStore = wrapped . dataStore
instance (IsDataStore a, Default l) => IsDataStore (Resizable l a) where fromDataStore = Resizable def . fromDataStore



-- === TF Instances ===

type instance ContainerOf (Resizable l a) = Resizable l a

instance IsContainer (Resizable l a) where fromContainer = id
instance HasContainer2 (Resizable l a) where container2 = id


instance HasContainer (Resizable l a) (Resizable l a) where container = id

type instance ElementOf        (Resizable l a) = ElementOf       a
type instance ElementByIx  idx (Resizable l a) = ElementByIx idx a
type instance IndexOf      el  (Resizable l a) = IndexOf     el  a

type instance DataStoreOf (Resizable l a) = DataStoreOf a
--type instance ModsOf (Resizable l a) inst = '[]


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf MeasurableQSM (Resizable l a) = ModsOf MeasurableQSM a
type instance ModsOf MinIndexedQSM (Resizable l a) = ModsOf MinIndexedQSM a
type instance ModsOf MaxIndexedQSM (Resizable l a) = ModsOf MaxIndexedQSM a

instance MeasurableQM q m a => MeasurableQSM (Resizable l a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM'     . unwrap
instance MinIndexedQM q m a => MinIndexedQSM (Resizable l a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unwrap
instance MaxIndexedQM q m a => MaxIndexedQSM (Resizable l a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unwrap



-- === Construction ===

-- [+] Singleton
-- [ ] Allocable
-- [ ] Expandable
-- [ ] Growable

type instance ModsOf SingletonQSM (Resizable l a) = ModsOf SingletonQSM a
instance (SingletonQM el q m a, Default l) => SingletonQSM el (Resizable l a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'






--instance Monad m  => MinIndexed (Resizable l a) m q s where minIndex _ _ _ = simple 0
--instance Monad m  => MaxIndexed (Resizable l a) m q s where maxIndex _ _   = (fmap.fmap) pred . sizeM


--class Measurable               cont m q s where sizeQSM      :: info ~ MeasurableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s Int)

--xxx :: forall info cont q s m. info ~ MeasurableInfo cont => Query q s -> info -> cont -> m (ResultBySel info s Int)
--xxx _ _ = queried (Proxy :: Proxy q) sizeM

--yyy :: forall q t m s info. (MeasurableFinalT q m t) => Query q s -> info -> t -> m (ResultByQuery (MeasurableInfo (ContainerOf t)) q Int)
--yyy _ _ = queried (Proxy :: Proxy q) sizeM
---- === Finite ===

---- [+] Measurable
---- [+] MinIndexed
---- [+] MaxIndexed

--instance I.MeasurableT q a sizeQSM => Measurable q mods (Resizable l a) sizeQSM where sizeQSM     spec = sizeQSM     (polySpecX spec) . unwrap
--instance I.MinIndexedT q a idx  => MinIndexed q mods (Resizable l a) idx  where minIndex spec = minIndex (polySpecX spec) . unwrap
--instance I.MaxIndexedT q a idx  => MaxIndexed q mods (Resizable l a) idx  where maxIndex spec = maxIndex (polySpecX spec) . unwrap

------ === Construction ===

------ [+] Singleton
------ [+] Allocable
------ [+] Growable
------ [+] Expandable

--instance (Default s, I.SingletonT  q a el)                       => Singleton  q mods (Resizable l a) el  where singleton spec   = wrap . singleton (polySpecX spec)
--instance (Default s, I.AllocableT  q a   )                       => Allocable  q mods (Resizable l a)     where alloc     spec   = wrap . alloc     (polySpecX spec)
--instance (I.GrowableT q a a', Result q s a' out)                 => Growable   q mods (Resizable l a) out where growQSM      spec   = runWrapped1 spec growQSM
--instance (I.GrowableT q a a', Result q s a' out, ResizeStep s a) => Expandable q mods (Resizable l a) out where expand    spec c = growQSM (rebaseSpecX spec) (resizeStep c) c


--class ExpandableF'               cont m q s where expandF'   :: (Monad m, info ~ (RawInfo ExpandableF' cont)) => Query q s -> ExpandableInfo' cont -> cont -> m (ResultF' info s)
--nestedLens :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))

--type instance ModsOf (Resizable l a) ExpandableF' = ModsOf a ExpandableF'
--instance ExpandableF' (Resizable l a) m q s where expandF' q i = nestedLens wrapped $ expandF' (freeQuery q) (freeInfo i)
--instance ExpandableF' a m q s => ExpandableF' (Resizable l a) m q s where expandF' q i (Resizable l a) = expandF' (Query) Info a
--instance ExpandableF' a m q s => ExpandableF' (Resizable l a) m q s where expandF' q i = nestedLens wrapped $ runModsF (Proxy :: Proxy q) expandFX i

--type instance ModsOf ExpandableF' (V.Vector a)   = '[Ixed ]
--instance Monad m  => ExpandableF' (V.Vector a) m q '[False] where expandF' _ _ v = simple $ runST $ V.unsafeFreeze =<< flip MV.unsafeGrow 1 =<< V.unsafeThaw v
--expandM :: ExpandableFinalT q m t => Func' q (t -> m (ResultByQuery (ExpandableInfo (ContainerOf t)) q t))

--type family ResultBySel (info :: *) (s :: [Bool]) where ResultBySel (Info idx el cls cont) s = ResultByQuery (Info idx el cls cont) (Selected s (ModsOf cls cont))

--type family ResultBySel (info :: *) (s :: [Bool]) where ResultBySel (Info idx el cls cont) s = ResultByQuery (Info idx el cls cont) (Selected s (FilterMutable (ModsOf cls cont)))
--type MatchResults i s i' q' = (ResultBySel i s ~ ResultByQuery i' q')

        --type instance ModsOf ExpandableQSM (Resizable l a) = ModsOf ExpandableQSM a
        --instance (ExpandableFinalT q m a, MatchResultsCls ExpandableInfo (Resizable l a) a s q) => ExpandableQSM (Resizable l a) m q s where expandQSM q i = nestedLens wrapped $ queried (Proxy :: Proxy q) expandM


--expandM :: (ExpandableFinalT q m t, Simplified ExpandableInfo q t a) => Func' q (t -> m a)


--m (ResultBySel (ExpandableInfo (Resizable l a)) s (Resizable l a))

--class Expandable               cont m q s where expand    :: info ~ ExpandableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s cont)


--class ExpandableF'               cont m q s where expandF'    :: info ~ ExpandableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s cont)
--class AppendableF'            el cont m q s where appendF'    :: info ~ AppendableInfo el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
--class SingletonF'             el cont m q s where singletonF' :: info ~ SingletonInfo  el cont => Query q s -> info -> el         -> m (ResultBySel info s cont)

--type XXX l a m q s = SubOperation (RawInfo Expandable (l a)) s (RawInfo Expandable a) q m

--instance (MatchResults (RawInfo ExpandableF' (Resizable l a)) s (RawInfo ExpandableF' a) q m) => ExpandableF' (Resizable l a) m q s where expandF' q i = nestedLens wrapped $ mytst4' (Proxy :: Proxy q)
--instance XXX (Resizable l) a m q s => ExpandableF' (Resizable l a) m q s where expandF' q i = nestedLens wrapped $ mytst4' (Proxy :: Proxy q)

--type family ResultF' (info :: *) (s :: [Bool]) where ResultF' (Info idx el cls cont) s = ResultF (Selected s (ModsOf cont cls)) (Info idx el cls cont) cont
                                                  --m (ResultF' (RawInfo ExpandableF' cont) s)

                                                  --m (ResultF (Selected s (ModsOf cont ExpandableF')) (ExpandableInfo' (Resizable l a)) (Resizable l a))
--tstx :: _ => Query q s -> i -> (Resizable l a) -> m (ResultF q                                       (ExpandableInfo' a)               (Resizable l a))
--tstx :: (Monad m, s' ~ LstIn mods q, mods ~ ModsOf a ExpandableF', ainfo ~ ExpandableInfo' a, aresult ~ ResultF q ainfo, Functor aresult, ExpandableF' a m q s', ResultF (Selected s' mods) ainfo ~ aresult, ResultF (Selected s mods) (RawInfo ExpandableF' (Resizable l a)) ~ aresult)
--     => Query q s -> ExpandableInfo' (Resizable l a) -> (Resizable l a) -> m (ResultF' (RawInfo ExpandableF' (Resizable l a)) s)
                                                                       --m (ResultF' (RawInfo ExpandableF' (Resizable l a)) s)

--ResultF' (Info idx el cls cont) s = ResultF (Selected s (ModsOf cont cls)) (Info idx el cls cont) cont

--mytst4' :: (ContOperation q info m, info ~ (RawInfo ExpandableF' t)) => Proxy (q :: [*]) -> t -> m (ResultF q info t)
--mytst4' = flip runModsF expandFX

--tstx ::                                                                                    Query q s -> ExpandableInfo' t -> t             -> m (ResultF' info s)
--tstx ::                                                                                    Query q s -> ExpandableInfo' t -> t             -> m (ResultF (Selected s (ModsOf cont cls)) (Info idx el cls cont) cont)


--type MatchResults i s q' i' m c = (ResultF' i s ~ ResultF q' i' c, ContOperation q' i' m)
--tstx :: (MatchResults i s q' (RawInfo ExpandableF' a) m (Resizable l a)) => Proxy (q' :: [*]) -> Query q s -> i                 -> Resizable l a -> m (ResultF' i s)


--type MatchResults i s q' i' c = (ResultF' i s ~ ResultF q' i' c)
--tstx :: (ContOperation q' i' m, i' ~ (RawInfo ExpandableF' a), result ~ ResultF' i s, result' ~ ResultF q' i' (Resizable l a), result ~ result') => Proxy (q' :: [*]) -> Query q s -> i                 -> Resizable l a -> m result

--class ExpandableF'               cont m q s where expandF'   :: (Monad m, info ~ (RawInfo ExpandableF' cont)) => Query q s -> ExpandableInfo' cont -> cont -> m (ResultF' info s)


 --(Monad m, info ~ (RawInfo ExpandableF' (c a))) =>                                                  Query q s  -> ExpandableInfo' (c a) -> (c a) -> m (ResultF' info s)

--tstx  :: (i' ~ (RawInfo ExpandableF' a), Wrapped c, MatchResults i s i' q', ContOperation q' i' m) => Proxy (q' :: [*]) -> (Query q s) -> i                     -> (c a) -> m (ResultBySel i s (c a))
--tstx q' q i = nestedLens wrapped $ mytst4' q'
--tstx q' (q :: Query q s) i = nestedLens wrapped $ flip runModsF expandFX q'
--runModsF (Proxy :: Proxy '[]) expandFX'
------ === Concatenation ===
------ [+] Appendable
------ [+] Prependable
------ [+] Addable
------ [+] Removable

----instance (AppendableX q (LstIn (ModsOf a AppendableX) q) a el, Result2 (In I.Ixed q) (Resizable s) (MonoResultEl q a el) (MonoResultEl q (Resizable l a) el) ) => AppendableX  q mods (Resizable l a) el where appendx spec = runWrapped1 spec appendx


--instance (I.AppendableT  q a el a', Result q s a' out) => Appendable  q mods (Resizable l a) el out where append  spec = runWrapped1 spec append
--instance (I.PrependableT q a el a', Result q s a' out) => Prependable q mods (Resizable l a) el out where prepend spec = runWrapped1 spec prepend
--instance (I.AddableT     q a el a', Result q s a' out) => Addable     q mods (Resizable l a) el out where add     spec = runWrapped1 spec add
--instance (I.RemovableT   q a el a', Result q s a' out) => Removable   q mods (Resizable l a) el out where remove  spec = runWrapped1 spec remove


---- === Modification ===

---- [+] Indexable
---- [+] Insertable
---- [ ] Reservable
---- [ ] Releasable

--instance  I.IndexableT  q a idx el                                                      => Indexable  q mods (Resizable l a) idx el     where index  spec idx   = index (polySpecX spec) idx . unwrap
--instance (I.InsertableT q a idx el a', Resize s a idx, Result2' q (Resizable s) a' out) => Insertable q mods (Resizable l a) idx el out where insert spec idx a = runWrapped2 spec insert idx a . resize idx


---- === Indexing ===

---- [+] TracksElems
---- [+] TracksIxes
---- [-] TracksFreeIxes
---- [-] TracksUsedIxes

--instance I.TracksElemsT q a elems => TracksElems q mods (Resizable l a) elems where elems   spec = elems   (polySpecX spec) . unwrap
--instance I.TracksIxesT  q a ixes  => TracksIxes  q mods (Resizable l a) ixes  where indexes spec = indexes (polySpecX spec) . unwrap


------------------------------------


data Minimal     = Minimal   deriving (Show)
data Exponential = Exponential deriving (Show)

instance Default Minimal     where def = Minimal
instance Default Exponential where def = Exponential

----class Resize2 style cont where
----    resizeAmount :: Proxy style -> cont -> Int

--class                                                                                               Resize style       cont idx where resize     :: idx -> Resizable style cont -> Resizable style cont
--instance (                       I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Minimal     cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ ((-) `on` fromEnum) idx $ I.maxIndex c else c
--instance (I.Measurable cont Int, I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Exponential cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ dupCheckSize (fromEnum idx) (I.sizeQSM c) - I.sizeQSM c else c

--class                             ResizeStep style       cont where resizeStep :: Resizable style cont -> Int
--instance                          ResizeStep Minimal     cont where resizeStep c = 1
--instance I.Measurable cont Int => ResizeStep Exponential cont where resizeStep c = checkZeroSize $ I.sizeQSM c


--checkZeroSize s = if s == 0 then 1 else s

--dupCheckSize i = dupSize i . checkZeroSize

--dupSize i sizeQSM = if i >= sizeQSM then dupSize i (2 * sizeQSM)
--                              else sizeQSM


--isOverBounds :: (Ord idx, I.MaxIndexed cont idx, HasContainer t cont) => idx -> t -> Bool
--isOverBounds idx cont = idx > I.maxIndex cont
