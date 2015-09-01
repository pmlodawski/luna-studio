{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Containers.Reusable where

import Prologue              hiding (Indexable, index, Bounded)
import Data.Containers.Class
import Data.Typeable

data HeteroReusable idx a = HeteroReusable [idx] !a deriving (Show)

type Reusable a = HeteroReusable (IndexOf' a) a

makeLenses ''HeteroReusable

type instance ElementOf (HeteroReusable idx a) = ElementOf a
type instance IndexOf el (HeteroReusable idx a) = IndexOf el a
type instance ElementByIx idx (HeteroReusable idx a) = ElementByIx idx a

instance Unwrap (HeteroReusable idx) where unwrap (HeteroReusable _ cont) = cont

instance HasContainer (HeteroReusable idx a) (HeteroReusable idx a) where container = id


type instance ElementOf       (HeteroReusable idx a) = ElementOf a
type instance ElementByIx idx (HeteroReusable idx a) = ElementByIx idx a
type instance IndexOf      el (HeteroReusable idx a) = IndexOf el a

instance Monoid a => Monoid (HeteroReusable idx a) where
    mempty                                                    = HeteroReusable mempty mempty
    mappend (HeteroReusable idxs a) (HeteroReusable idxs' a') = HeteroReusable (idxs <> idxs') (a <> a')

instance Bounded a idx => Bounded (HeteroReusable idx a) idx where
    minBoundIdx = minBoundIdx . unwrap
    maxBoundIdx = maxBoundIdx . unwrap


instance Wrapped (HeteroReusable idx) where
    wrapped = lens (\(HeteroReusable _ a) -> a) (\(HeteroReusable idxs _) a -> HeteroReusable idxs a)



instance (idx ~ idx')                                          => Sparse              (HeteroReusable idx a) idx'    where freeIxs      (HeteroReusable free _) = free
instance (idx ~ idx', Expandable' a idx)                       => Reservable          (HeteroReusable idx a) idx'    where reserveIx  c@(HeteroReusable free a) = case free of
                                                                                                                                                                  (f:fs) -> (f, HeteroReusable fs a)
                                                                                                                                                                  []     -> reserveIx $ expand c
instance (Insertable a idx el, Expandable' a idx)              => SetLike             (HeteroReusable idx a)      el
instance (idx ~ idx', Insertable a idx el, Expandable' a idx)  => SetLike'            (HeteroReusable idx a) idx' el where add' el c = (c' & wrapped %~ insert idx el, idx) where
                                                                                                                                       (idx, c') = reserveIx c

instance (idx ~ idx', Erasable' a idx el)                      => Erasable            (HeteroReusable idx a) idx'
instance (idx ~ idx', Erasable' a idx el)                      => Erasable'           (HeteroReusable idx a) idx' el where unsafeErase' idx (HeteroReusable free a) = (HeteroReusable (idx : free) a', el) where
                                                                                                                                                                      (a', el) = unsafeErase' idx a


instance Measurable a                                          => Measurable          (HeteroReusable idx a)         where size                    = size    . unwrap
instance Container a idx el                                    => Container           (HeteroReusable idx a) idx  el where elems                   = elems   . unwrap
                                                                                                                           indexes                 = indexes . unwrap

instance {-# OVERLAPPABLE #-} (Expandable' a idx)             => Expandable           (HeteroReusable idx a)         where expand = fst . expand'
instance {-# OVERLAPPABLE #-} (Expandable' a idx, idx ~ idx') => Expandable'          (HeteroReusable idx a) idx'    where expand' (HeteroReusable idxs a) = (HeteroReusable (idxs <> idxs') a', idxs') where
                                                                                                                                                             (a', idxs') = expand' a

instance (Allocable  a, Container a idx el)                   => Allocable            (HeteroReusable idx a)         where unsafeAlloc i = HeteroReusable (indexes cont) cont where
                                                                                                                                           cont = unsafeAlloc i


instance (idx ~ idx', Indexable2 '[Unsafe2] a idx el) => Indexable2 '[Unsafe2] (HeteroReusable idx a) idx' el where index2 opts idx = index2 opts idx . unwrap
