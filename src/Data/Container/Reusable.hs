{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Container.Reusable where

import Prologue              hiding (Indexable, index, Bounded, Ixed)
import Data.Container.Class
import Data.Typeable
import qualified Data.Container.Interface as I
import           Data.Container.Poly {- x -}
import qualified Data.Container.Mods as Mods

import Data.Container.Parametrized
import           Data.Layer
import           Data.List ((\\))

-- Types

data HReusable idx a = HReusable [idx] !a deriving (Show, Functor, Traversable, Foldable)
type Reusable  m     = Parametrized (HReusable (HomoIndexOf m)) m



type instance ContainerOf (HReusable idx a) = HReusable idx a

instance IsContainer  (HReusable idx a) where fromContainer = id
instance HasContainer (HReusable idx a) where container     = id

type instance ElementOf        (HReusable idx a) = ElementOf       a
type instance ElementByIx  idx (HReusable idx a) = ElementByIx idx a
type instance IndexOf      el  (HReusable idx a) = IndexOf     el  a


type instance DataStoreOf (HReusable idx a) = DataStoreOf a
instance HasDataStore a => HasDataStore (HReusable idx a) where dataStore = layered . dataStore
instance IsDataStore a => IsDataStore (HReusable idx a) where fromDataStore = HReusable def . fromDataStore

instance Default a => Default (HReusable idx a) where def = HReusable def def


-- Layers

type instance Unlayered (HReusable idx a) = a
instance Layered (HReusable idx a) where
    layered = lens (\(HReusable ixs a) -> a) (\(HReusable ixs _) a -> HReusable ixs a)

--instance unlayer (HReusable idx) where unlayer (HReusable _ cont) = cont
--instance Wrap   (HReusable idx) where wrap                           = HReusable def

--instance layered (HReusable idx) where
--    layered = lens (\(HReusable _ a) -> a) (\(HReusable idxs _) a -> HReusable idxs a)

-- Instances

instance Monoid a => Monoid (HReusable idx a) where
    mempty                                          = HReusable mempty mempty
    mappend (HReusable idxs a) (HReusable idxs' a') = HReusable (idxs <> idxs') (a <> a')


type instance Item (HReusable idx a) = Item a
instance FromList a => FromList (HReusable idx a) where
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

instance MeasurableQM q m a => MeasurableQSM (HReusable idx a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM'     . unlayer
instance MinIndexedQM idx q m a => MinIndexedQSM idx (HReusable idx a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unlayer
instance MaxIndexedQM idx q m a => MaxIndexedQSM idx (HReusable idx a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unlayer


-- === Construction ===

-- [+] Singleton
-- [ ] Allocable
-- [+] Expandable
-- [+] Growable

type instance ModsOf SingletonQSM  (HReusable idx a) = ModsOf SingletonQSM a
type instance ModsOf ExpandableQSM (HReusable idx a) = ModsOf ExpandableQSM a
type instance ModsOf GrowableQSM   (HReusable idx a) = ModsOf GrowableQSM a

        --instance SingletonQM el q m a => SingletonQSM el (HReusable idx a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'

instance (Monad m, ExpandableQM (Mods.Ixed ': q) m a, idx ~ IndexOf' (DataStoreOf a)) => ExpandableQSM (HReusable idx a) m q s where
    expandQSM _ _ c = do
        (ixs, r) <- splitResData <$> nested layered ((ixed . queried (Proxy :: Proxy q)) expandM') c
        return $ fmap (withIxes' (<> ixs)) r

instance (Monad m, GrowableQM (Mods.Ixed ': q) m a, idx ~ IndexOf' (DataStoreOf a)) => GrowableQSM (HReusable idx a) m q s where
    growQSM _ _ i c = do
        (ixs, r) <- splitResData <$> nested layered ((ixed . queried (Proxy :: Proxy q)) growM' i) c
        return $ fmap (withIxes' (<> ixs)) r


-- === Modification ===

-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable
-- [+] Insertable

type instance ModsOf AppendableQSM (HReusable idx a) = ModsOf AppendableQSM a
type instance ModsOf AddableQSM    (HReusable idx a) = ModsOf InsertableQSM a

instance AppendableQM el q m a => AppendableQSM el (HReusable idx a) m q s where appendQSM _ _ el c = nested layered (queried (Proxy :: Proxy q) appendM' el) c

instance (AddableQM el q m (HReusable idx a), InsertableQM idx el q m a, TransCheck q (InsertableInfo idx el) (AddableInfo el) a, Expandable (HReusable idx a)) => AddableQSM el (HReusable idx a) m q s where
    addQSM q i el c@(HReusable ixs a) = case ixs of (x:xs) -> fmap2 (withIxes tail) $ nested layered (queried (Proxy :: Proxy q) insertM' x el) c
                                                    []     -> queried (Proxy :: Proxy q) addM' el $ expand c


---- === Indexing ===

-- [+] Indexable
-- [ ] TracksElems
-- [ ] TracksIxes
-- [+] TracksFreeIxes
-- [ ] TracksUsedIxes


type instance ModsOf IndexableQSM      (HReusable idx a) = ModsOf IndexableQSM   a
type instance ModsOf TracksElemsQSM    (HReusable idx a) = '[]
type instance ModsOf TracksFreeIxesQSM (HReusable idx a) = '[]
type instance ModsOf TracksIxesQSM     (HReusable idx a) = ModsOf TracksIxesQSM  a

instance (idx' ~ idx, IndexableQM  idx el q m a) => IndexableQSM      idx' el (HReusable idx a) m q s where indexQSM    _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unlayer
instance (idx' ~ idx, TracksIxesQM idx    q m a) => TracksIxesQSM     idx'    (HReusable idx a) m q s where ixesQSM     _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer
instance (idx' ~ idx, Monad m)                   => TracksFreeIxesQSM idx'    (HReusable idx a) m q s where freeIxesQSM _ _ (HReusable ixs _) = return $ Result () ixs


instance   ( TracksIxes      idx    (HReusable idx a)
           , TracksFreeIxes  idx    (HReusable idx a)
           , Indexable       idx el (HReusable idx a)
           , Eq              idx
           , Monad           m
           ) => TracksElemsQSM el (HReusable idx a) m q s where elemsQSM _ _ c = simpleM els where
                                                                    ixs = ixes c \\ freeIxes c :: [idx]
                                                                    els = fmap (flip index c) ixs
