{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Data.Containers.Resizable where

import Prologue              hiding (Indexable, index, Bounded, Simple, Ixed)
import Data.Containers.Class
import Data.Typeable
import qualified Data.Containers.Interface as I
import           Data.Containers.Poly {- x -}
import           Data.TypeLevel.List (In)


import           Data.Vector         (Vector)


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
instance HasContainer (Resizable l a) where container = id


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
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ModsOf SingletonQSM (Resizable l a) = ModsOf SingletonQSM a
instance (SingletonQM el q m a, Default l) => SingletonQSM el (Resizable l a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'

type instance ModsOf AllocableQSM (Resizable l a) = ModsOf AllocableQSM a
instance (AllocableQM q m a, Default l) => AllocableQSM (Resizable l a) m q s where allocQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) allocM'

type instance ModsOf ExpandableQSM (Resizable l a) = ModsOf GrowableQSM a
instance (GrowableQM q m a, TransCheck q GrowableInfo ExpandableInfo a, ResizeStep l a) => ExpandableQSM (Resizable l a) m q s where expandQSM _ _ c = nestedLens wrapped (queried (Proxy :: Proxy q) growM' (resizeStep c)) c

type instance ModsOf GrowableQSM (Resizable l a) = ModsOf GrowableQSM a
instance GrowableQM q m a => GrowableQSM (Resizable l a) m q s where growQSM _ _    = nestedLens wrapped . queried (Proxy :: Proxy q) growM'


-- === Modification ===

-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable

type instance ModsOf AppendableQSM (Resizable l a) = ModsOf AppendableQSM a
type instance ModsOf InsertableQSM (Resizable l a) = ModsOf InsertableQSM a

instance   AppendableQM     el q m a => AppendableQSM     el (Resizable l a) m q s where appendQSM _ _ = nestedLens wrapped .  queried (Proxy :: Proxy q) appendM'
instance   InsertableQM idx el q m a => InsertableQSM idx el (Resizable l a) m q s where insertQSM _ _ = nestedLens wrapped .: queried (Proxy :: Proxy q) insertM'



---- === Indexing ===

-- [+] Indexable
-- [ ] TracksElems
-- [ ] TracksIxes
-- [ ] TracksFreeIxes
-- [ ] TracksUsedIxes


type instance ModsOf IndexableQSM (Resizable l a) = ModsOf IndexableQSM a

instance   IndexableQM idx el q m a => IndexableQSM idx el (Resizable l a) m q s where indexQSM _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unwrap



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

class                                                              ResizeStep style       cont where resizeStep :: Resizable style cont -> Int
instance                                                           ResizeStep Minimal     cont where resizeStep _ = 1
instance MeasurableQM '[] Identity (Resizable Exponential cont) => ResizeStep Exponential cont where resizeStep   = checkZeroSize . size


checkZeroSize s = if s == 0 then 1 else s

--dupCheckSize i = dupSize i . checkZeroSize

--dupSize i sizeQSM = if i >= sizeQSM then dupSize i (2 * sizeQSM)
--                              else sizeQSM


--isOverBounds :: (Ord idx, I.MaxIndexed cont idx, HasContainer t cont) => idx -> t -> Bool
--isOverBounds idx cont = idx > I.maxIndex cont







-- ---- TODO ----
-- after doing it we could be able to optimize the premise of ResizeStep Exponential and make inference nicer
--
-- -- Optimize following use cases:
--
-- xxx :: (MeasurableQM2 '[] Identity t, (ResultX
--                         (Info NA NA MeasurableQSM2 (DataStoreOf t))
--                         (Selected
--                            (LstIn (ModsOf MeasurableQSM2 t) '[])
--                            (FilterMutable (ModsOf MeasurableQSM2 t)))
--                       ~ ()),
--
-- DataFillable
--                                 '[]
--                                 (TaggedCont
--                                    (Selected
--                                       (LstIn (ModsOf MeasurableQSM2 t) '[])
--                                       (FilterMutable (ModsOf MeasurableQSM2 t)))
--                                    ()), (Taggable
--                         (Selected
--                            (LstIn (ModsOf MeasurableQSM2 t) '[])
--                            (FilterMutable (ModsOf MeasurableQSM2 t)))
--                         ())) => Resizable style t -> Int
--
-- --- in particular:
--
-- (Selected
--                            (LstIn (ModsOf MeasurableQSM2 t) '[])
--                            (FilterMutable (ModsOf MeasurableQSM2 t)))
--
-- -- should always return []