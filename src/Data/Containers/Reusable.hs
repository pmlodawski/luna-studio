{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Containers.Reusable where

import Prologue              hiding (Indexable, index, Bounded, Ixed)
import Data.Containers.Class
import Data.Typeable
import qualified Data.Containers.Interface as I
import           Data.Containers.Poly {- x -}


-- Types

data HReusable idx a = HReusable [idx]       !a deriving (Show)
type Reusable      a = HReusable (IndexOf' a) a

instance HasContainer (HReusable idx a) (HReusable idx a) where container = id

type instance ContainerOf (HReusable idx a) = HReusable idx a

instance IsContainer (HReusable idx a) where fromContainer = id
instance HasContainer2 (HReusable idx a) where container2 = id

type instance ElementOf        (HReusable idx a) = ElementOf       a
type instance ElementByIx  idx (HReusable idx a) = ElementByIx idx a
type instance IndexOf      el  (HReusable idx a) = IndexOf     el  a

type instance DataStoreOf (HReusable idx a) = DataStoreOf a
instance HasDataStore a => HasDataStore (HReusable idx a) where dataStore = wrapped . dataStore
instance IsDataStore a => IsDataStore (HReusable idx a) where fromDataStore = HReusable def . fromDataStore


-- Wrappers

instance Unwrap (HReusable idx) where unwrap (HReusable _ cont) = cont
instance Wrap   (HReusable idx) where wrap                           = HReusable def

instance Wrapped (HReusable idx) where
    wrapped = lens (\(HReusable _ a) -> a) (\(HReusable idxs _) a -> HReusable idxs a)

-- Instances

instance Monoid a => Monoid (HReusable idx a) where
    mempty                                                    = HReusable mempty mempty
    mappend (HReusable idxs a) (HReusable idxs' a') = HReusable (idxs <> idxs') (a <> a')

instance IsList a => IsList (HReusable idx a) where
    type Item (HReusable idx a) = Item a
    fromList = HReusable mempty . fromList

-- Utils

withIxes_ :: ([idx] -> (r, [idx'])) -> HReusable idx a -> (r, HReusable idx' a)
withIxes_ f (HReusable ixs a) = (out, HReusable ixs' a) where
    (out, ixs') = f ixs

withIxes :: ([idx] -> [idx']) -> HReusable idx a -> HReusable idx' a
withIxes = flattenMod withIxes_

withIxes' :: ([idx] -> [idx]) -> HReusable idx a -> HReusable idx a
withIxes' = withIxes

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf MeasurableQSM (HReusable idx a) = ModsOf MeasurableQSM a
type instance ModsOf MinIndexedQSM (HReusable idx a) = ModsOf MinIndexedQSM a
type instance ModsOf MaxIndexedQSM (HReusable idx a) = ModsOf MaxIndexedQSM a

instance MeasurableQM q m a => MeasurableQSM (HReusable idx a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM'     . unwrap
instance MinIndexedQM q m a => MinIndexedQSM (HReusable idx a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unwrap
instance MaxIndexedQM q m a => MaxIndexedQSM (HReusable idx a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unwrap




-- === Construction ===

-- [+] Singleton
-- [ ] Allocable
-- [ ] Expandable
-- [ ] Growable

type family Foo (a :: [*]) :: [*]

type instance ModsOf SingletonQSM (HReusable idx a) = ModsOf SingletonQSM a
instance SingletonQM el q m a => SingletonQSM el (HReusable idx a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'
--instance SingletonQM el q m a => SingletonQSM el (HReusable idx a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'

--tstf :: (SingletonQM q opts m t) => Proxy opts
--                 -> q
--                 -> m (ResultByQuery
--                         (Info NA q SingletonQSM (DataStoreOf (ContainerOf t))) opts t)
--tstf :: (SingletonQM el opts m t) => Proxy opts -> el -> m (ResultByQuery (SingletonInfo el (DataStoreOf (ContainerOf t))) opts t)
--tstf :: _ => Proxy (opts :: [*]) -> el -> m (a,b)
--tstf q v = (ixed . queried q) singletonM' v



    --type instance ModsOf ExpandableQSM2 (HReusable idx a) = ModsOf ExpandableQSM2 a
    --instance (Monad m, ExpandableQM2 (Ixed ': q) m a, idx ~ IndexOf' (DataStoreOf a)) => ExpandableQSM2 (HReusable idx a) m q s where
    --    expandQSM2 _ _ c = do
    --        (ixs, r) <- splitResData <$> nestedLens wrapped ((ixed . queried (Proxy :: Proxy q)) expandM2') c
    --        return $ fmap (withIxes' (<> ixs)) r


--withIxes :: ([idx] -> [idx']) -> HReusable idx a -> HReusable idx' a


--splitResData :: ResW (d,ds) r -> (d, ResW ds r)


-- Utils

--freeAllIxes :: I.TracksIxes a [idx] => HReusable t a -> HReusable idx a
--freeAllIxes (HReusable _ a) = HReusable (I.indexes a) a



-- === Finite ===

-- [+] Measurable
-- [+] MinIndexed
-- [+] MaxIndexed

--instance I.MeasurableT q a size => Measurable q mods (HReusable idx a) size where size     spec = size     (polySpecX spec) . unwrap
--instance I.MinIndexedT q a idx  => MinIndexed q mods (HReusable idx a) idx  where minIndex spec = minIndex (polySpecX spec) . unwrap
--instance I.MaxIndexedT q a idx  => MaxIndexed q mods (HReusable idx a) idx  where maxIndex spec = maxIndex (polySpecX spec) . unwrap


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Growable
-- [ ] Expandable

--instance (I.SingletonT  q a el, I.TracksIxes a [idx]) => Singleton  q mods (HReusable idx a) el                         where singleton spec   = freeAllIxes . wrap . singleton (polySpecX spec)
--instance (I.AllocableT  q a   , I.TracksIxes a [idx]) => Allocable  q mods (HReusable idx a)                            where alloc     spec   = freeAllIxes . wrap . alloc     (polySpecX spec)

--type instance ModsOf (HReusable idx a)     Growable = '[Ixed ]
--instance (I.GrowableT '[Ixed] a ([idx], a'), IsContainer a) => Growable q '[False] (HReusable idx a)           (HReusable idx a') where grow      spec i (HReusable ixs a) = (HReusable (ixs <> ixs') a') where
--                                                                                                                                                                                            (ixs', a') = ixed I.grow i a

--instance (I.GrowableT '[Ixed] a ([idx], a'), IsContainer a) => Growable q '[True ] (HReusable idx a)    ([idx], HReusable idx a') where grow      spec i (HReusable ixs a) = (ixs', HReusable (ixs <> ixs') a') where
--                                                                                                                                                                                            (ixs', a') = ixed I.grow i a


-- === Concatenation ===
-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable

--instance I.AppendableT  q a el a' => Appendable  q mods (HReusable idx a) el (HReusable idx a') where append  spec el = wrapped %~ append  (polySpecX spec) el
--instance I.PrependableT q a el a' => Prependable q mods (HReusable idx a) el (HReusable idx a') where prepend spec el = wrapped %~ prepend (polySpecX spec) el
--instance I.AddableT     q a el a' => Addable     q mods (HReusable idx a) el (HReusable idx a') where add     spec el = wrapped %~ add     (polySpecX spec) el
--instance I.RemovableT   q a el a' => Removable   q mods (HReusable idx a) el (HReusable idx a') where remove  spec el = wrapped %~ remove  (polySpecX spec) el

--instance (I.Insertable a idx el a', I.ExpandableT '[Ixed] a ([idx], a), IsContainer a) => Addable q mods (HReusable idx a) el (HReusable idx a') where
--    add spec el (HReusable ixs a) = case ixs of (x:xs) -> HReusable xs  $ I.insert x  el a
--                                                     []     -> HReusable xs' $ I.insert x' el a' where
--                                                               (x':xs', a') = ixed I.expand a


-- === Modification ===

-- [+] Indexable
-- [+] Insertable
-- [ ] Reservable
-- [ ] Releasable

--instance  I.IndexableT  q a idx el                     => Indexable  q mods (HReusable idx a) idx el                  where index  spec idx   = index (polySpecX spec) idx . unwrap
--instance (I.InsertableT q a idx el a', Resize s a idx) => Insertable q mods (HReusable idx a) idx el (HReusable idx a') where insert spec idx a = (wrapped %~ insert (polySpecX spec) idx a) . resize idx


-- === Indexing ===

-- [-] TracksElems
-- [+] TracksIxes
-- [+] TracksFreeIxes
-- [-] TracksUsedIxes

--instance I.TracksElemsT q a elems => TracksElems q mods (HReusable idx a) elems where elems   spec = elems   (polySpecX spec) . unwrap
--instance I.TracksIxesT  q a ixes  => TracksIxes     q mods (HReusable idx a) ixes  where indexes  spec                     = indexes (polySpecX spec) . unwrap
--instance           (ixes ~ [idx]) => TracksFreeIxes q mods (HReusable idx a) ixes  where freeIxes _ (HReusable ixs _) = ixs



