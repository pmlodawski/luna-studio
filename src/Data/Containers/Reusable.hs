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

data HeteroReusable idx a = HeteroReusable [idx]       !a deriving (Show)
type Reusable           a = HeteroReusable (IndexOf' a) a

instance HasContainer (HeteroReusable idx a) (HeteroReusable idx a) where container = id

type instance ElementOf        (HeteroReusable idx a) = ElementOf       a
type instance ElementByIx  idx (HeteroReusable idx a) = ElementByIx idx a
type instance IndexOf      el  (HeteroReusable idx a) = IndexOf     el  a

-- Wrappers

instance Unwrap (HeteroReusable idx) where unwrap (HeteroReusable _ cont) = cont
instance Wrap   (HeteroReusable idx) where wrap                           = HeteroReusable def

instance Wrapped (HeteroReusable idx) where
    wrapped = lens (\(HeteroReusable _ a) -> a) (\(HeteroReusable idxs _) a -> HeteroReusable idxs a)

-- Instances

instance Monoid a => Monoid (HeteroReusable idx a) where
    mempty                                                    = HeteroReusable mempty mempty
    mappend (HeteroReusable idxs a) (HeteroReusable idxs' a') = HeteroReusable (idxs <> idxs') (a <> a')

instance IsList a => IsList (HeteroReusable idx a) where
    type Item (HeteroReusable idx a) = Item a
    fromList = HeteroReusable mempty . fromList

-- Utils

--freeAllIxes :: I.TracksIxes a [idx] => HeteroReusable t a -> HeteroReusable idx a
--freeAllIxes (HeteroReusable _ a) = HeteroReusable (I.indexes a) a



-- === Finite ===

-- [+] Measurable
-- [+] MinIndexed
-- [+] MaxIndexed

--instance I.MeasurableT q a size => Measurable q mods (HeteroReusable idx a) size where size     spec = size     (polySpecX spec) . unwrap
--instance I.MinIndexedT q a idx  => MinIndexed q mods (HeteroReusable idx a) idx  where minIndex spec = minIndex (polySpecX spec) . unwrap
--instance I.MaxIndexedT q a idx  => MaxIndexed q mods (HeteroReusable idx a) idx  where maxIndex spec = maxIndex (polySpecX spec) . unwrap


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Growable
-- [ ] Expandable

--instance (I.SingletonT  q a el, I.TracksIxes a [idx]) => Singleton  q mods (HeteroReusable idx a) el                         where singleton spec   = freeAllIxes . wrap . singleton (polySpecX spec)
--instance (I.AllocableT  q a   , I.TracksIxes a [idx]) => Allocable  q mods (HeteroReusable idx a)                            where alloc     spec   = freeAllIxes . wrap . alloc     (polySpecX spec)

--type instance ModsOf (HeteroReusable idx a)     Growable = '[Ixed ]
--instance (I.GrowableT '[Ixed] a ([idx], a'), IsContainer a) => Growable q '[False] (HeteroReusable idx a)           (HeteroReusable idx a') where grow      spec i (HeteroReusable ixs a) = (HeteroReusable (ixs <> ixs') a') where
--                                                                                                                                                                                            (ixs', a') = ixed I.grow i a

--instance (I.GrowableT '[Ixed] a ([idx], a'), IsContainer a) => Growable q '[True ] (HeteroReusable idx a)    ([idx], HeteroReusable idx a') where grow      spec i (HeteroReusable ixs a) = (ixs', HeteroReusable (ixs <> ixs') a') where
--                                                                                                                                                                                            (ixs', a') = ixed I.grow i a


-- === Concatenation ===
-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable

--instance I.AppendableT  q a el a' => Appendable  q mods (HeteroReusable idx a) el (HeteroReusable idx a') where append  spec el = wrapped %~ append  (polySpecX spec) el
--instance I.PrependableT q a el a' => Prependable q mods (HeteroReusable idx a) el (HeteroReusable idx a') where prepend spec el = wrapped %~ prepend (polySpecX spec) el
--instance I.AddableT     q a el a' => Addable     q mods (HeteroReusable idx a) el (HeteroReusable idx a') where add     spec el = wrapped %~ add     (polySpecX spec) el
--instance I.RemovableT   q a el a' => Removable   q mods (HeteroReusable idx a) el (HeteroReusable idx a') where remove  spec el = wrapped %~ remove  (polySpecX spec) el

--instance (I.Insertable a idx el a', I.ExpandableT '[Ixed] a ([idx], a), IsContainer a) => Addable q mods (HeteroReusable idx a) el (HeteroReusable idx a') where
--    add spec el (HeteroReusable ixs a) = case ixs of (x:xs) -> HeteroReusable xs  $ I.insert x  el a
--                                                     []     -> HeteroReusable xs' $ I.insert x' el a' where
--                                                               (x':xs', a') = ixed I.expand a


-- === Modification ===

-- [+] Indexable
-- [+] Insertable
-- [ ] Reservable
-- [ ] Releasable

--instance  I.IndexableT  q a idx el                     => Indexable  q mods (HeteroReusable idx a) idx el                  where index  spec idx   = index (polySpecX spec) idx . unwrap
--instance (I.InsertableT q a idx el a', Resize s a idx) => Insertable q mods (HeteroReusable idx a) idx el (HeteroReusable idx a') where insert spec idx a = (wrapped %~ insert (polySpecX spec) idx a) . resize idx


-- === Indexing ===

-- [-] TracksElems
-- [+] TracksIxes
-- [+] TracksFreeIxes
-- [-] TracksUsedIxes

--instance I.TracksElemsT q a elems => TracksElems q mods (HeteroReusable idx a) elems where elems   spec = elems   (polySpecX spec) . unwrap
instance I.TracksIxesT  q a ixes  => TracksIxes     q mods (HeteroReusable idx a) ixes  where indexes  spec                     = indexes (polySpecX spec) . unwrap
instance           (ixes ~ [idx]) => TracksFreeIxes q mods (HeteroReusable idx a) ixes  where freeIxes _ (HeteroReusable ixs _) = ixs



