{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Data.Container.Resizable where

import Prologue              hiding (Indexable, index, Bounded, Simple, Ixed)
import Data.Container.Class
import Data.Typeable
import qualified Data.Container.Interface as I
import           Data.Container.Poly {- x -}
import           Data.TypeLevel.List (In)


import           Data.Vector         (Vector)
import           Data.Container.Parametrized



-- === HResizable wrapper

data HResizable style a = HResizable !style !a deriving (Show, Functor, Traversable, Foldable)
type Resizable t = Parametrized (HResizable t)


instance              Unwrap  (HResizable l) where  unwrap (HResizable _ a) = a
instance Default l => Wrap    (HResizable l) where    wrap = HResizable def
instance              Wrapped (HResizable l) where wrapped = lens unwrap $ \(HResizable l _) a -> HResizable l a


instance (Default l, Monoid a) => Monoid (HResizable l a) where
    mempty                                   = HResizable def mempty
    mappend (HResizable l a) (HResizable _ a') = HResizable l (a <> a')

instance (Default l, Monoid a) => Default (HResizable l a) where
    def = HResizable def mempty

instance (IsList a, Default l) => IsList (HResizable l a) where
    type Item (HResizable l a) = Item a
    fromList = HResizable def . fromList

instance HasDataStore a => HasDataStore (HResizable l a) where dataStore = wrapped . dataStore
instance (IsDataStore a, Default l) => IsDataStore (HResizable l a) where fromDataStore = HResizable def . fromDataStore



-- === TF Instances ===

type instance ContainerOf (HResizable l a) = HResizable l a

instance IsContainer (HResizable l a) where fromContainer = id
instance HasContainer (HResizable l a) where container = id


type instance ElementOf        (HResizable l a) = ElementOf       a
type instance ElementByIx  idx (HResizable l a) = ElementByIx idx a
type instance IndexOf      el  (HResizable l a) = IndexOf     el  a

type instance DataStoreOf (HResizable l a) = DataStoreOf a


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded


type instance ModsOf MeasurableQSM (HResizable l a) = ModsOf MeasurableQSM a
type instance ModsOf MinIndexedQSM (HResizable l a) = ModsOf MinIndexedQSM a
type instance ModsOf MaxIndexedQSM (HResizable l a) = ModsOf MaxIndexedQSM a

instance MeasurableQM q m a => MeasurableQSM (HResizable l a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM'     . unwrap
instance MinIndexedQM q m a => MinIndexedQSM (HResizable l a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unwrap
instance MaxIndexedQM q m a => MaxIndexedQSM (HResizable l a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unwrap


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ModsOf SingletonQSM (HResizable l a) = ModsOf SingletonQSM a
instance (SingletonQM el q m a, Default l) => SingletonQSM el (HResizable l a) m q s where singletonQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) singletonM'

type instance ModsOf AllocableQSM (HResizable l a) = ModsOf AllocableQSM a
instance (AllocableQM q m a, Default l) => AllocableQSM (HResizable l a) m q s where allocQSM _ _    = (fmap . fmap) wrap . queried (Proxy :: Proxy q) allocM'

type instance ModsOf ExpandableQSM (HResizable l a) = ModsOf GrowableQSM a
instance (GrowableQM q m a, TransCheck q GrowableInfo ExpandableInfo a, ResizeStep l a) => ExpandableQSM (HResizable l a) m q s where expandQSM _ _ c = nestedLens wrapped (queried (Proxy :: Proxy q) growM' (resizeStep c)) c

type instance ModsOf GrowableQSM (HResizable l a) = ModsOf GrowableQSM a
instance GrowableQM q m a => GrowableQSM (HResizable l a) m q s where growQSM _ _    = nestedLens wrapped . queried (Proxy :: Proxy q) growM'


-- === Modification ===

-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable

type instance ModsOf AppendableQSM (HResizable l a) = ModsOf AppendableQSM a
type instance ModsOf InsertableQSM (HResizable l a) = ModsOf InsertableQSM a

instance   AppendableQM     el q m a => AppendableQSM     el (HResizable l a) m q s where appendQSM _ _ = nestedLens wrapped .  queried (Proxy :: Proxy q) appendM'
instance   InsertableQM idx el q m a => InsertableQSM idx el (HResizable l a) m q s where insertQSM _ _ = nestedLens wrapped .: queried (Proxy :: Proxy q) insertM'



---- === Indexing ===

-- [+] Indexable
-- [ ] TracksElems
-- [ ] TracksIxes
-- [ ] TracksFreeIxes
-- [ ] TracksUsedIxes


type instance ModsOf IndexableQSM (HResizable l a) = ModsOf IndexableQSM a

instance   IndexableQM idx el q m a => IndexableQSM idx el (HResizable l a) m q s where indexQSM _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unwrap



------------------------------------


data Minimal     = Minimal   deriving (Show)
data Exponential = Exponential deriving (Show)

instance Default Minimal     where def = Minimal
instance Default Exponential where def = Exponential

----class Resize2 style cont where
----    resizeAmount :: Proxy style -> cont -> Int

--class                                                                                               Resize style       cont idx where resize     :: idx -> HResizable style cont -> HResizable style cont
--instance (                       I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Minimal     cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ ((-) `on` fromEnum) idx $ I.maxIndex c else c
--instance (I.Measurable cont Int, I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Exponential cont idx where resize idx c = if isOverBounds idx c then flip I.growQSM c $ dupCheckSize (fromEnum idx) (I.sizeQSM c) - I.sizeQSM c else c

--class                             ResizeStep style       cont where resizeStep :: HResizable style cont -> Int
--instance                          ResizeStep Minimal     cont where resizeStep c = 1
--instance I.Measurable cont Int => ResizeStep Exponential cont where resizeStep c = checkZeroSize $ I.sizeQSM c

class                                                              ResizeStep style       cont where resizeStep :: HResizable style cont -> Int
instance                                                           ResizeStep Minimal     cont where resizeStep _ = 1
instance MeasurableQM '[] Identity (HResizable Exponential cont) => ResizeStep Exponential cont where resizeStep   = checkZeroSize . size


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
--                         ())) => HResizable style t -> Int
--
-- --- in particular:
--
-- (Selected
--                            (LstIn (ModsOf MeasurableQSM2 t) '[])
--                            (FilterMutable (ModsOf MeasurableQSM2 t)))
--
-- -- should always return []